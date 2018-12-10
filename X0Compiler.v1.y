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
    variable,
};

enum data_type_enum {
	integer,
	boolean,
	string_chars,
	single_char,
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
enum object cur_kind;
char cur_id[MAX_VAL_LEN];
int cur_table_adr;
int cur_type;
int do_begin_pos;
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
char fname[MAX_SYM_TABLE];
int err;
extern int line; 

void init();
void enter(enum object k);
int position(char *s);
void gen(enum fct x, int y, char *z);
void listall();
void displaytable();
//void interpret();
int base(int l, int* s, int b);

#define INT_TYPE 101
#define BOOL_TYPE 102
#define CHAR_TYPE 103
#define STRING_TYPE 104

int const_label = 0;
int declaration_init = 0;

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

%right BECOMES
%left AND OR
%left XOR
%left EQL NEQ
%left GTR LSS LEQ GEQ 
%left PLUS MINUS
%left TIMES SLASH MOD
%right INC DEC NOT

%nonassoc UMINUS 
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSESYM

%token <ident> IDENT
%token <number> NUMBER
%token <string_chars> STRING
%token <single_char> CHAR
%token <boolean> BOOL

%type <type_val> simple_expr factor
%type <number> get_code_addr

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
							return -1; // error code.
						}
						else {	// var declaration part without initialation
							declaration_init = 0;
							strcpy(id, $1); 
							enter(variable);
							/* need classification for different data type 
							switch (cur_type) {
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
						if (cur_type != $3) {
							yyerror("Mismatched in data type!\n");
						}
						declaration_init = 1;
						if (const_label == 1) {	// const declaration part
							strcpy(id, $1); 
							enter(constant);
							/* need classification for different data type */
						}
						else {	// var declaration part with initialation
							declaration_init = 1;
							strcpy(id, $1); 
							enter(variable);

							int id_addr = 0;
							enum data_type_enum type_tmp;
							for (int i = 1; i <= sym_tab_tail; i++) {
								if (strcmp($1, table[i].name) == 0) {
									id_addr = table[i].adr;
									break;
								}
							}
							gen(sto, 0, (char*)id_addr);
						}	
					}
				;

type:				INTSYM {cur_type = INT_TYPE;}
				|	CHARSYM	{cur_type = CHAR_TYPE;}
				|	STRINGSYM {cur_type = STRING_TYPE;}
				|	BOOL {cur_type = BOOL_TYPE;}
				;

compound_stat:		LBPAREN
					statement_list 
					RBPAREN
				;

statement_list:		statement_list statement
				|	statement
				;

statement:			expression_stat
				|	compound_stat
				|	select_stat
				|	iteration_stat
				|	read_stat
				|	write_stat	
				;

expression_stat:	expression_stat COMMA expression SEMICOLON
				|	expression SEMICOLON 
				;

expression:			var BECOMES expression {
						if (cur_kind == constant) {
							yyerror("Illegal assignment on constant!\n");
						}
						if ($1 != $3) {
							yyerror("Mismatched in data type!\n");
						}
						gen(sto, 0, (char*)cur_table_adr);
						table[cur_table_adr].val = ; // expr value;
						$$ = 0; // undefined
					}
				|	simple_expr {
						$$ = $1;
					}
				|	{	// undefined;

				} 
				;

var:				IDENT {
						strcmp(cur_id, $1);
						int i, exist_flag = 0;
						for (i = 1; i <= sym_tab_tail; ++i) {
							if (strcmp(cur_id, table[i].name) == 0) {
								exist_flag = 1;
								cur_table_adr = i;
								$$ = table[i].type;
								cur_kind = table[i].kind;
								cur_type = table[i].type;
								break;
							}
						}
						if (exist_flag == 0) {
							yyerror("Undefinded variable!\n");
						}
					}
				;

select_stat:		if_stat
				|	switch_stat
				;

if_stat:			IFSYM LPAREN expression RPAREN 
					statement	%prec LOWER_THAN_ELSE
				|	IFSYM LPAREN expression RPAREN 
					statement
					ELSESYM
					statement
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

for_stat:			FORSYM LPAREN expression SEMICOLON get_code_addr 
					expression SEMICOLON get_code_addr {
						int opr_tmp = 0;
						gen(jpc, 0, (char*)opr_tmp); // true --- jump out $8
						gen(jmp, 0, (char*)opr_tmp); // false --- jump to state $8+1
					}
					expression RPAREN get_code_addr {
						int adr_tmp = $5;
						gen(jmp, 0, (char*)adr_tmp); // back to condition part
					}
					statement {

					}
				;

while_stat:			WHILESYM LPAREN get_code_addr expression RPAREN get_code_addr {
						int opr_tmp = 0;
						gen(jpc, 0, (char*)opr_tmp);
					} 
					statement {
						int while_begin_pos = $3, while_jmp_pos = $6, while_back_patch_pos;
						gen(jmp, 0, (char*)while_begin_pos);
						while_back_patch_pos = VM_pointer;
						memcpy((void*)code[while_jmp_pos].val, (const void*)&while_back_patch_pos, MAX_VAL_LEN);
					}
				;

get_code_addr:		{$$ = VM_pointer};

do_while_stat:		DOSYM {
						do_begin_pos = VM_pointer;
					}
					statement 
					WHILESYM LPAREN
					expression
					RPAREN SEMICOLON {
						int opr_tmp = 0;
						gen(jpc, 0, (char*)opr_tmp); // jump out
						gen(jmp, 0, (char*)do_begin_pos); // back to begin part
						memcpy((void*)code[VM_pointer - 2].adr, (const void*)&VM_pointer, MAX_VAL_LEN); // complete jump out adr
					}
				;

read_stat:			READSYM LPAREN read_list RPAREN SEMICOLON
				;

read_list:	 		read_list COMMA read_var
				|	read_var
				;

read_var:			var {
						if (cur_kind == constant) {
							yyerror("Operation on contant data!\n");
						}
						gen(opr, cur_type, (char*)READ_OPR); // OPR undefined;
						gen(opr, cur_type, (char*)cur_adr);
					}
				;

write_stat:			WRITESYM LPAREN write_list RPAREN SEMICOLON
				;

write_list:			write_list COMMA expression {
						int opr_tmp = WRITE_OPR;
						gen(opr, 0, opr_tmp);
					}
				|	expression {
						int opr_tmp = WRITE_OPR;
						gen(opr, 0, opr_tmp);
					}
				;

simple_expr: 		unary_expr {
						$$ = $1;
					}
				| 	simple_expr binary_opr unary_expr {
						if ($1 != $3) {
							yyerror("Mismatched types in two operands!\n");
						}
						opr_tmp = $2;
						gen(opr, 0, (char*)opr_tmp);
					}
				|	LPAREN simple_expr RPAREN {
						$$ = $2;
					}
				| 	NOT simple_expr {
						$$ = $2;
					}
				|	MINUS simple_expr %prec UMINUS
				;

unary_expr: 		unary_opr unary_expr 
				|	unary_exprII
				;

unary_exprII: 		unary_exprII unary_opr 
				| 	factor
				;



unary_opr:			INC
				|	DEC
				;

binary_opr:			PLUS
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
						$$ = INT_TYPE;
						input_val_int = $1;
						gen(lit, INT_TYPE, (char*)&input_val_int);
					}
				|	BOOL {
						$$ = BOOL_TYPE;
						input_val_bool = $1;
						gen(lit, BOOL_TYPE, (char*)&input_val_bool);
					}
				|	STRING {
						$$ = STRING_TYPE;
						strcpy(input_val_string, $1);
						gen(lit, CHAR_TYPE, (char*)&input_val_char);
					}
				|	CHAR {
						$$ = CHAR_TYPE;
						input_val_char = $1;
						gen(lit, STRING_TYPE, (char*)&input_val_string);
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
		switch (cur_type) {
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
				printf("Unknown data type in constant enter part!");
		}
	}
	else if (k == variable) {
		switch (cur_type) {
			case INT_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_int, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].adr = cur_adr++;
				break;
			case BOOL_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_bool, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].adr = cur_adr++;
				break;
			case CHAR_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_char, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].adr = cur_adr++;
				break;
			case STRING_TYPE:
				if (declaration_init == 1) {
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_string, MAX_VAL_LEN);
					table[sym_table_tail].init_lable = 1;
				}
				table[sym_table_tail].adr = cur_adr++;
				break;
			default:
				printf("Unknown data type in variable enter part!");
		}
	}
	else printf("Unknown sym_table enter operation!");
}

/* 查找标识符在符号表中的位置 */
int position(char *s) {
	int i, res = -1;
	for (i = 1; i <= sym_table_tail; ++i) {
		if (strcmp(table[i].name, s) != 0) {
			res = table[i].adr;
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
	memcpy((void*)(code[VM_pointer].val), (const void*)z, MAX_VAL_LEN);
	VM_pointer++;
}

/* 输出所有目标代码  */
void listall() {
	int i;
	char name[][5]= {
		{"lit"},{"opr"},{"lod"},{"sto"},{"cal"},{"int"},{"jmp"},{"jpc"},
	};
	if (listall_switch_on) {
		for (i = 0; i < VM_pointer; i++) {
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
				switch (cur_type) {
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
				switch (cur_type) {
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



int main(int argc, char* argv[]) {

	if ((fin = fopen(argv[1], "r")) == NULL) {
		printf("Can't open the source code file!\n");
		exit(1);
	}

	if ((fcode = fopen("fcode.txt", "w+")) == NULL) {
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
	extern int yydebug;
	yydebug = 1;
  	yyparse();

	displaytable();	/* 输出符号表 */
	listall();  /* 输出所有代码 */
	//interpret();	/* 调用解释执行程序 */        	
	fclose(fin);
	fclose(fcode);
	fclose(ftable);
 	fclose(foutput);
	return 0;
}