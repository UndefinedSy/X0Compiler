//声明部分
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <malloc.h>

#define bool int
#define true 1
#define false 0

#define MAX_SYM_TABLE   100     /* 符号表容量 */
#define MAX_AL          10         /* 标识符的最大长度 */
#define MAX_ADD         2048     /* 地址上界*/
#define MAX_DEPTH       3      /* 最大允许过程嵌套声明层数*/
#define MAX_CODE        200     /* 最多的虚拟机代码数 */
#define MAX_STACK       500 /* 运行时数据栈元素最多为500个 */
#define MAX_VAL_LEN		114	/*无类型数据长度*/

enum object {
    constant,
    varbiable,
};

enum data_type_enum {
	integer
	boolean,
	string_chars,
	single_char,
};

enum var_type {
    integer,
    char_single,
    chars,
    boolean,
};

struct tablestruct {
    char name[MAX_AL];
    enum object kind;
	enum data_type_enum type;
    char val[MAX_VAL_LEN];
    int adr;
    int size;
	int init_lable;
}table[MAX_SYM_TABLE];

enum fct {
    lit,     opr,     lod, 
    sto,     cal,     ini, 
    jmp,     jpc,  
};

struct instruction {
    enum fct f;
    int l;
    char val[MAX_VAL_LEN];
}code[MAX_CODE];

int sym_table_tail;	/* 符号表当前尾指针 */
int VM_pointer;         /* 虚拟机代码指针, 取值范围[0, cxmax-1] */
char id[MAX_AL];
int num;
int cur_adr = 3;
int input_val_int;
bool input_val_bool;
char input_val_char;
char input_val_string[MAX_VAL_LEN];
bool listall_switch_on;   /* 显示虚拟机代码与否 */
bool sym_table_switch_on;  /* 显示符号表与否 */

FILE* fin;      /* 输入源文件 */
FILE* ftable;	  /* 输出符号表 */
FILE* fcode;    /* 输出虚拟机代码 */
FILE* foutput;  /* 输出出错示意（如有错） */
FILE* fresult;  /* 输出执行结果 */
char fname[al];
int err;
extern int line; 

void init();
void enter(enum object k);
int position(char *s);
void gen(enum fct x, int y, int z);
void listall();
void displaytable();
void interpret();
int base(int l, int* s, int b);

#define INT_TYPE 101
#define BOOL_TYPE 102
#define CHAR_TYPE 103
#define STRING_TYPE 104

int const_label = 0;
int declaration_init = 0;
int data_type = 0;

%}

//辅助定义部分
%union {
    char *ident;
    int number;
	int boolean;
	int type_val;
	char *string_chars;
	char single_char;
	struct {
		int sym, dim;
	} arr;
}

%token BOOLSYM BREAKSYM CASESYM CHARSYM CONSTSYM CONTINUESYM DOSYM DEFAULTSYM ELSESYM FORSYM IFSYM INTSYM
%token MAINSYM READSYM REPEATSYM SWITCHSYM STRINGSYM THENSYM UNSIGNEDSYM UNTILSYM WHILESYM WRITESYM RETURNSYM
%token BECOMES LSS LEQ GTR GEQ EQL NEQ PLUS INC MINUS DEC TIMES SLASH MOD LPAREN RPAREN LBRACK
%token RBRACK LBPAREN RBPAREN AND OR NOT XOR COMMA PERIOD COLON SEMICOLON

%token <ident> IDENT
%token <number> NUMBER
%token <string_chars> STRING
%token <single_char> CHAR
%token <boolean> BOOL


%type <type_val> simple_expr factor

%nonassoc UMINUS

////////////////////////////////////////////////////////
//规则部分
%%

program:            MAINSYM
                    LBPAREN
                    declaration_list
                    statement_list
                    RBPAREN
                ;

declaration_list:	declaration_list declaration_stat
                |   declaration_stat
                |   
                ;

declaration_stat:	var_declaration
				|	const_declaration
				;

var_declaration:	type  
					ident_list 
					SEMICOLON
				;	

const_declaration:	CONSTSYM {
						const_label = 1;
					}	 
					type ident_list {
						const_label = 0;
					}
					SEMICOLON
				;

ident_list:			ident_def
				|	ident_list COMMA ident_def
				;

ident_def:			IDENT {
						if (const_label == 1) {	// const declaration without initialization.
							yyerror("Const declaration requires initialization!\n");
							return CONST_ERROR; // error code.
						}
						else {	// var declaration part without initialation
							declaration_init = 0;
							strcpy(id, $1); 
							enter(variable);
							/* need classification for different data type 
							switch (data_type) {
								case INT_TYPE:
									// ... solution
									break;
								case BOOL_TYPE:
									// ... solution
									break;
								case CHAR_TYPE:
									// ... solution
									break;
								case STRING_TYPE:
									// ... solution
									break;
									
							}
							*/

						}
					}
				|	IDENT BECOMES simple_expr {
						declaration_init = 1;
						if (const_label = 1) {	// const declaration part
							strcpy(id, $1); 
							enter(constant);
							/* need classification for different data type */
						}
						else {	// var declaration part with initialation
							strcpy(id, $1); 
							enter(variable);
						}
					}
				;

type:				INTSYM {data_type = INT_TYPE;}
				|	CHARSYM	{data_type = CHAR_TYPE;}
				|	STRINGSYM {data_type = STRING_TYPE;}
				|	BOOL {data_type = BOOL_TYPE;}
				;

compound_stat:		LBPAREN
					statement_list 
					RBPAREN
				;

statement_list:		statement_list statement
				|	statement
				|
				;

statement:			expression_stat
				|	compound_stat
				|	select_stat
				|	iteration_stat
				|	read_stat
				|	write_stat	
				;

expression_stat:	expression_stat expression SEMICOLON
				|	expression SEMICOLON
				|
				;

expression:			var BECOMES expression
				|	simple_expr
				|							// solve (var) 
				;

var:				IDENT 
				;

select_stat:		if_stat
				|	switch_stat
				;

if_stat:			IFSYM LPAREN expression RPAREN 
					statement
					else_stat
				;

else_stat:			ELSESYM
					statement
				|	
				;	

switch_stat:		SWITCHSYM 
					LPAREN expression RPAREN 
					LBPAREN
					case_list
					default_stat
					RBPAREN
				;

case_list:			CASESYM
					expression
					COLON
					statement
				;

default_stat: 		DEFAULTSYM COLON 
					statement
				|
				;

iteration_stat:		for_stat
				|	while_stat
				|	do_while_stat
				;

for_stat:			FORSYM LPAREN 
					expression 
					SEMICOLON 
					expression 
					SEMICOLON 
					expression 
					RPAREN 
					statement
				;

while_stat:			WHILESYM LPAREN 
					expression 
					RPAREN
					statement
				;

do_while_stat:		DOSYM
					statement
					WHILESYM LPAREN
					expression
					RPAREN SEMICOLON
				;

read_stat:			READSYM LPAREN read_list RPAREN SEMICOLON
				;

read_list:	 		read_list COMMA read_var
				|	read_var
				;

read_var:			var
				;

write_stat:			WRITESYM LPAREN write_list RPAREN SEMICOLON
				;

write_list:			write_list COMMA expression
				|	expression
				;

simple_expr:		factor
				|	MINUS simple_expr %prec UMINUS
				| 	unary_opr simple_expr
				| 	simple_expr unary_opr
				| 	simple_expr opr simple_expr
				| 	NOT simple_expr
				| 	LPAREN simple_expr RPAREN
				;

unary_opr:			INC
				|	DEC
				;

opr:				PLUS
				|	MINUS
				|	TIMES
				|	SLASH
				|	MOD
				|	AND
				|	OR
				|	XOR
				|	LSS
				|	LEQ
				|	GTR
				|	GEQ
				|	EQL
				|	NEQ
				;

factor:				NUMBER {
						$$.type = INT_TYPE;
						input_val_int = $1;
						gen(lit, INT_TYPE, (char*)&input_val_int));
					}
				|	BOOL {
						$$.type = BOOL_TYPE;
						input_val_bool = $1;
						gen(lit, BOOL_TYPE, (char*)&input_val_bool));
					}
				|	STRING {
						$$.type = STRING_TYPE;
						strcpy(input_val_string, $1);
						gen(lit, CHAR_TYPE, (char*)&input_val_char));
					}
				|	CHAR {
						$$.type = CHAR_TYPE;
						input_val_char = $1;
						gen(lit, STRING_TYPE, (char*)&input_val_string));
					}
				| 	var {

					}
				;

////////////////////////////////////////////////////////
//程序部分
%%
int yyerror(char *s) {
	err = err + 1;
  	printf("%s in line %d\n", s, line);
	fprintf(foutput, "%s in line %d\n", s, line);
	return 0;
}

/* 初始化 */
void init() {
	sym_table_tail = 0;
	VM_pointer = 0;
  	num = 0;
  	err = 0;
	input_val_int = 0;
	input_val_bool = 0;
	input_val_char = 0;
	strcpy(input_val_string, "\0");
}

/* 在符号表中加入一项 */
void enter(enum object k) {
	sym_table_tail++;
	strcpy(table[sym_table_tail].name, id);
	table[sym_table_tail].kind = k;
	table[sym_table_tail].init_lable = declaration_init;
	if (k == constant) {
		switch (data_type) {
			case INT_TYPE:
				table[sym_table_tail].type = integer;
				memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_int, MAX_VAL_LEN);
				break;
			case BOOL_TYPE:
				memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_bool, MAX_VAL_LEN);
				break;
			case CHAR_TYPE:
				memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_char, MAX_VAL_LEN);
				break;
			case STRING_TYPE:
				memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_string, MAX_VAL_LEN);
				break;
			default:
				printf("Unknown data type in constant enter part!")
		}
	}
	else if (k == variable) {
		switch (data_type) {
			case INT_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_int, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].addr = cur_adr++;
				break;
			case BOOL_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_bool, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].addr = cur_adr++;
				break;
			case CHAR_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_char, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].addr = cur_adr++;
				break;
			case STRING_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_string, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].addr = cur_adr++;
				break;
			default:
				printf("Unknown data type in variable enter part!")
		}
	}
	else printf("Unknown sym_table enter operation!")
}

/* 查找标识符在符号表中的位置 */
int position(char *s) {
	int i, res = -1;
	for (i = 1; i <= sym_table_tail; ++i) {
		if (strcmp(table[i].name, s) != 0) {
			res = table[i].addr;
			break;
		}
	}
	return res;
}

/* 生成虚拟机代码 */
void gen(enum fct x, int y, char *z) {
	if (VM_pointer >= MAX_CODE) {
		printf("Program is too long!\n");	/* 生成的虚拟机代码程序过长 */
		exit(1);
	}
	code[VM_pointer].f = x;
	code[VM_pointer].l = y;
	memcpy((void*)(code[vm_code_pointer].opr), (const void*)z, STRING_LEN);
	VM_pointer++;
}

/* 输出所有目标代码  */
void listall() {
	int i;
	char name[][5]= {
		{"lit"},{"opr"},{"lod"},{"sto"},{"cal"},{"int"},{"jmp"},{"jpc"},
	};
	if (listall_switch_on) {
		for (i = 0; i < vm_code_pointer; i++) {
			if (code[i].f == lit){
				switch (code[i].l) {
					case INT_TYPE:
						printf("%d %s %d %d\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val));
						fprintf(fcode,"%d %s %d %d\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val));
						break;
					case BOOL_TYPE:
						printf("%d %s %d %s\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val) == 1 ? "true" : "false");
						fprintf(fcode,"%d %s %d %s\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val) == 1 ? "true" : "false");
						break;
					case CHAR_TYPE: // test required.
						printf("%d %s %d %c\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val));
						fprintf(fcode,"%d %s %d %c\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val));
						break;
					case STRING_TYPE:
						printf("%d %s %d %s\n", i, name[code[i].f], code[i].l, code[i].val);
						fprintf(fcode,"%d %s %d %s\n", i, name[code[i].f], code[i].l, code[i].val);
						break;
				}
			}
			else { // uncleared
				printf("%d %s %d %d\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val));
				fprintf(fcode,"%d %s %d %d\n", i, name[code[i].f], code[i].l, *((int*)&code[i].val));
			}			
		}
	}
}

/* 输出符号表 */
void displaytable() {
	int i;
	printf(" idx\tkind\ttype\tname\tval\n");
	fprintf(ftable, " idx\tkind\ttype\tname\tval\n");
	if (sym_table_switch_on) {		/* 输出符号表 */
	
		for (i = 1; i <= sym_table_tail; ++i) {
			if (table[i].kind == constant) {
				switch (data_type) {
					case INT_TYPE:
						printf("%4d\tconst\tint\t%20s\t%d\n", i, table[i].name, *((int*)&table[i].val));
						fprintf(ftable, "%4d\tconst\tint\t%20s\t%d\n", i, table[i].name, *((int*)&table[i].val));
						break;
					case BOOL_TYPE:
						printf("%4d\tconst\tbool\t%20s\t%s\n", i, table[i].name, *((int*)&table[i].val) == 1 ? "true" : "false");
						fprintf(ftable, "%4d\tconst\tbool\t%20s\t%s\n", i, table[i].name, *((int*)&table[i].val) == 1 ? "true" : "false");
						break;
					case CHAR_TYPE:
						printf("%4d\tconst\tchar\t%20s\t%c\n", i, table[i].name, *((char*)&table[i].val));
						fprintf(ftable, "%4d\tconst\tchar\t%20s\t%c\n", i, table[i].name, *((char*)&table[i].val));
						break;
					case STRING_TYPE:
						printf("%4d\tconst\tstring\t%20s\t%s\n", i, table[i].name, *((char*)&table[i].val));
						fprintf(ftable, "%4d\tconst\tstring\t%20s\t%s\n", i, table[i].name, *((char*)&table[i].val));
						break;
				}
			}
			else {	//	kind = var
				switch (data_type) {
					case INT_TYPE:
						printf("%4d\tvar\tint\t%20s\t%d\n", i, table[i].name, table[i].init_lable == 1 ? *((int*)&table[i].val) : 114514);
						fprintf(ftable, "%4d\tvar\tint\t%20s\t%d\n", i, table[i].name, table[i].init_lable == 1 ? *((int*)&table[i].val) : 114514);
						break;
					case BOOL_TYPE:
						printf("%4d\tvar\tbool\t%20s\t%s\n", i, table[i].name, table[i].init_lable == 1 ? (*((int*)&table[i].val) == 1 ? "true" : "false") : "114514");
						fprintf(ftable, "%4d\tvar\tbool\t%20s\t%s\n", i, table[i].name, table[i].init_lable == 1 ? (*((int*)&table[i].val) == 1 ? "true" : "false") : "114514");
						break;
					case CHAR_TYPE:
						printf("%4d\tvar\tchar\t%20s\t%c\n", i, table[i].name, table[i].init_lable == 1 ? *((char*)&table[i].val) : '-');
						fprintf(ftable, "%4d\tvar\tchar\t%20s\t%c\n", i, table[i].name, table[i].init_lable == 1 ? *((char*)&table[i].val) : '-');
						break;
					case STRING_TYPE:
						printf("%4d\tvar\tstring\t%20s\t%s\n", i, table[i].name, table[i].init_lable == 1 ? *((char*)&table[i].val) : "114514");
						fprintf(ftable, "%4d\tvar\tstring\t%20s\t%s\n", i, table[i].name, table[i].init_lable == 1 ? *((char*)&table[i].val) : "114514");
						break;
				}
			}
		}
		printf("\n");
		fprintf(ftable, "\n");
	}

}

/* 解释程序 */
void interpret() {
	int p = 0; /* 指令指针 */
	int b = 1; /* 指令基址 */
	int t = 0; /* 栈顶指针 */
	struct instruction i;	/* 存放当前指令 */
	int s[MAX_STACK];	/* 栈 */

	printf("Start pl0\n");
	fprintf(fresult,"Start pl0\n");
	s[0] = 0; /* s[0]不用 */
	s[1] = 0; /* 主程序的三个联系单元均置为0 */
	s[2] = 0;
	s[3] = 0;
	do {
	    i = code[p];	/* 读当前指令 */
		p = p + 1;
		switch (i.f) {
			case lit:	/* 将常量a的值取到栈顶 */
				t = t + 1;
				s[t] = i.a;				
				break;
			case opr:	/* 数学、逻辑运算 */
				switch (i.a) {
					case 0:  /* 函数调用结束后返回 */
						t = b - 1;
						p = s[t + 3];
						b = s[t + 2];
						break;
					case 1: /* 栈顶元素取反 */
						s[t] = - s[t];
						break;
					case 2: /* 次栈顶项加上栈顶项，退两个栈元素，相加值进栈 */
						t = t - 1;
						s[t] = s[t] + s[t + 1];
						break;
					case 3:/* 次栈顶项减去栈顶项 */
						t = t - 1;
						s[t] = s[t] - s[t + 1];
						break;
					case 4:/* 次栈顶项乘以栈顶项 */
						t = t - 1;
						s[t] = s[t] * s[t + 1];
						break;
					case 5:/* 次栈顶项除以栈顶项 */
						t = t - 1;
						s[t] = s[t] / s[t + 1];
						break;
					case 6:/* 栈顶元素的奇偶判断 */
						s[t] = s[t] % 2;
						break;
					case 8:/* 次栈顶项与栈顶项是否相等 */
						t = t - 1;
						s[t] = (s[t] == s[t + 1]);
						break;
					case 9:/* 次栈顶项与栈顶项是否不等 */
						t = t - 1;
						s[t] = (s[t] != s[t + 1]);
						break;
					case 10:/* 次栈顶项是否小于栈顶项 */
						t = t - 1;
						s[t] = (s[t] < s[t + 1]);
						break;
					case 11:/* 次栈顶项是否大于等于栈顶项 */
						t = t - 1;
						s[t] = (s[t] >= s[t + 1]);
						break;
					case 12:/* 次栈顶项是否大于栈顶项 */
						t = t - 1;
						s[t] = (s[t] > s[t + 1]);
						break;
					case 13: /* 次栈顶项是否小于等于栈顶项 */
						t = t - 1;
						s[t] = (s[t] <= s[t + 1]);
						break;
					case 14:/* 栈顶值输出 */
						printf("%d", s[t]);
						fprintf(fresult, "%d", s[t]);
						t = t - 1;
						break;
					case 15:/* 输出换行符 */
						printf("\n");
						fprintf(fresult,"\n");
						break;
					case 16:/* 读入一个输入置于栈顶 */
						t = t + 1;
						printf("?");
						fprintf(fresult, "?");
						scanf("%d", &(s[t]));
						fprintf(fresult, "%d\n", s[t]);						
						break;
				}
				break;
			case lod:	/* 取相对当前过程的数据基地址为a的内存的值到栈顶 */
				t = t + 1;
				s[t] = s[base(i.l,s,b) + i.a];				
				break;
			case sto:	/* 栈顶的值存到相对当前过程的数据基地址为a的内存 */
				s[base(i.l, s, b) + i.a] = s[t];
				t = t - 1;
				break;
			case cal:	/* 调用子过程 */
				s[t + 1] = base(i.l, s, b);	/* 将父过程基地址入栈，即建立静态链 */
				s[t + 2] = b;	/* 将本过程基地址入栈，即建立动态链 */
				s[t + 3] = p;	/* 将当前指令指针入栈，即保存返回地址 */
				b = t + 1;	/* 改变基地址指针值为新过程的基地址 */
				p = i.a;	/* 跳转 */
				break;
			case ini:	/* 在数据栈中为被调用的过程开辟a个单元的数据区 */
				t = t + i.a;	
				break;
			case jmp:	/* 直接跳转 */
				p = i.a;
				break;
			case jpc:	/* 条件跳转 */
				if (s[t] == 0) 
					p = i.a;
				t = t - 1;
				break;
		}
	} while (p != 0);
	printf("End pl0\n");
	fprintf(fresult,"End pl0\n");
}


int main(int argc, char* argv[]) {

	if ((fin = fopen(argv[1], "r")) == NULL) {
		printf("Can't open the source code file!\n");
	}

	if ((fcode = fopen("fcode.in", "w+")) == NULL) {
		printf("Can't open the fcode file!\n");
		exit(1);
	}	
	if ((foutput = fopen("foutput.txt", "w+")) == NULL) {
		printf("Can't open the output file!\n");
		exit(1);
	}
	if ((ftable = fopen("ftable.txt", "w+")) == NULL) {
		printf("Can't open ftable.txt file!\n");
		exit(1);
	}
	
	redirectInput(fin);		
	init();
  	yyparse();

	displaytable()	/* 输出符号表 */
	listall();  /* 输出所有代码 */
	//interpret();	/* 调用解释执行程序 */        	
	fclose(fin);
	fclose(fcode);
	fclose(ftable);
 	fclose(foutput);
	return 0;
}