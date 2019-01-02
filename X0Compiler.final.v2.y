//声明部分
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <malloc.h>
#include <ctype.h>

#define bool int
#define true 1
#define false 0

#define MAX_SYM_TABLE   100     /* 符号表容量 */
#define MAX_AL          10   	/* 标识符的最大长度 */
#define MAX_ADD         114514 	/* 地址上界*/
#define MAX_CODE        200    /* 最多的虚拟机代码数 */
#define MAX_STACK       114514 	/* 运行时数据栈元素最多为500个 */
#define MAX_VAL_LEN		114		/*无类型数据长度*/
#define MAX_DATA_STACK	11451	/*数据栈大小*/
#define MAX_ARRAY_DIM	7

enum object {
    constant_single,
    variable_single,
	constant_array,
	variable_array,
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
	int type;
    char val[MAX_VAL_LEN];
    int adr;
    int size;
	int array_lable;
	int array_dim_size[MAX_ARRAY_DIM];
	int init_lable;
}table[MAX_SYM_TABLE];

enum fct {
    lit,	opr,	lod, 
    sto,	lodb,	stob,
	ini,	jmp,	jpc,  	 
};

struct instruction {
    enum fct f;
    int l;
    char val[MAX_VAL_LEN];
}code[MAX_CODE];

struct data_stack{
	enum data_type_enum t;			
	char val[MAX_VAL_LEN];
}s[MAX_STACK];

int sym_table_tail = 0;	/* 符号表当前尾指针 */
int VM_pointer;         /* 虚拟机代码指针, 取值范围[0, cxmax-1] */
char id[MAX_AL];
enum object cur_kind;
char cur_id[MAX_VAL_LEN];
int cur_table_adr;
int cur_type;
int do_begin_pos;
int num;
int curr_address = 3;
int cur_adr;
int stack_top = 2;
int input_val_int;
int becomes_adr;
int adr_shift;
int arr_size;

struct dim_recoder {
	int idx_type; // 0 for num, 1 for var_adr
	int val;
};
int arr_dim_recoder[MAX_ARRAY_DIM];
int arr_dim_idx = 0;
struct dim_recoder tmp_dim_recoder[MAX_ARRAY_DIM];
int tmp_dim_idx = 0;
int tmp_type_lable;
int ndim_var_lable;
bool input_val_bool;
char input_val_char;
char input_val_string[MAX_VAL_LEN];
bool listall_switch_on = 1;   /* 显示虚拟机代码与否 */
bool sym_table_switch_on = 1;  /* 显示符号表与否 */

FILE* fin;      /* 输入源文件 */
FILE* ftable;	  /* 输出符号表 */
FILE* fcode;    /* 输出虚拟机代码 */
FILE* foutput;  /* 输出出错示意（如有错） */
FILE* fresult;  /* 输出执行结果 */
FILE* fdatastack;
char fname[MAX_SYM_TABLE];
int err;
extern int line; 

void init();
void enter(enum object k);
void gen(enum fct x, int y, char *z);
void listall();
void displaytable();
//void interpret();
int base(int l, int* s, int b);

#define INT_TYPE 101
#define BOOL_TYPE 102
#define CHAR_TYPE 103
#define STRING_TYPE 104

#define INC_OPR 201
#define DEC_OPR 202
#define NOT_OPR 203
#define PLUS_OPR 204
#define MINUS_OPR 205
#define TIMES_OPR 206
#define SLASH_OPR 207
#define MOD_OPR 208
#define AND_OPR 209
#define OR_OPR 210
#define XOR_OPR 211
#define LSS_OPR 212
#define LEQ_OPR 213
#define GTR_OPR 214
#define GEQ_OPR 215
#define EQL_OPR 216
#define NEQ_OPR 217
#define READ_OPR 218
#define WRITE_OPR 219
#define UMINUS_OPR 220
#define ODD_OPR 221
#define OFF_OPR 222
#define POP_OPR	223
#define CASE_EQL_OPR 224

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
	struct {
		int case_begin;
		int case_end;
	} case_recorder;
}

%token BOOLSYM BREAKSYM CASESYM CHARSYM CONSTSYM CONTINUESYM DOSYM DEFAULTSYM ELSESYM FORSYM IFSYM INTSYM
%token MAINSYM READSYM REPEATSYM SWITCHSYM STRINGSYM THENSYM UNSIGNEDSYM UNTILSYM WHILESYM WRITESYM RETURNSYM
%token BECOMES LSS LEQ GTR GEQ EQL NEQ PLUS INC MINUS DEC TIMES SLASH MOD LPAREN RPAREN LBRACK
%token RBRACK LBPAREN RBPAREN AND OR NOT XOR COMMA PERIOD COLON SEMICOLON ODDSYM

%right BECOMES
%left AND OR
%left XOR
%left EQL NEQ
%left GTR LSS LEQ GEQ 
%left PLUS MINUS
%left TIMES SLASH MOD
%right INC DEC NOT ODDSYM

%nonassoc UMINUS 
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSESYM

%token <ident> IDENT
%token <number> NUMBER
%token <string_chars> STRING
%token <single_char> CHAR
%token <boolean> BOOL

%type <type_val> expression if_stat for_stat
%type <type_val> simple_expr additive_expr multiplying_expr term factor 
%type <number> binary_bool_opr binary_add_opr binary_mul_opr unary_opr
%type <number> gen_jmp gen_jpc
%type <number> get_code_addr var
%type <number> dimension_list dimension CASESYM
%type <arr>	dimension_exp 
%type <case_recorder> case_stat

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
				|	var_array_declaration
				|	const_array_declaration
				;

var_declaration:	type ident_list SEMICOLON
				;	

const_declaration:	CONSTSYM set_const_label
					type ident_list {
						const_label = 0;
					}
					SEMICOLON
				;

var_array_declaration:	type ident_array_list SEMICOLON
				;

const_array_declaration: CONSTSYM set_const_label
						type ident_array_def {
							const_label = 0;
						}
						SEMICOLON
				;	
set_const_label:	{
						const_label = 1;
					}
				;

ident_array_list: 	ident_array_def
				|	ident_array_list COMMA ident_array_def
				;

ident_list:			ident_def
				|	ident_list COMMA ident_def
				;
ident_array_def: 	IDENT LBRACK dimension_list RBRACK {
						int i = 0, j = arr_dim_idx-1;
						arr_size = $3;
						stack_top += arr_size;
						strcpy(id, $1);
						declaration_init = 0;
						/*
						printf("CHECK POINT in array declaration: %s[", id);
						for (i = 0; i < arr_dim_idx; ++i) {
							if (i != arr_dim_idx-1) {
								printf("%d][", arr_dim_recoder[i]);
							}
							else {
								printf("%d]\n", arr_dim_recoder[i]);
							}
						}
						*/
						if (const_label == 1) {	// const declaration part
							enter(constant_array);
							/* need classification for different data type */
						}
						else {
							enter(variable_array);
						}
						memset(arr_dim_recoder, 0, sizeof(arr_dim_recoder));
						arr_dim_idx = 0;
						arr_size = 0;
					}
				;

dimension_list:		dimension {
						$$ = $1;
						arr_dim_recoder[arr_dim_idx++] = $1;
					}
				|	dimension_list RBRACK LBRACK dimension {
						$$ = $1 * $4;
						arr_dim_recoder[arr_dim_idx++] = $4;	
					}
				;

dimension:			NUMBER {
						$$ = $1;
					}
				;

ident_def:			IDENT {
						if (const_label == 1) {	// const declaration without initialization.
							yyerror("Const declaration requires initialization!\n", 1001);
							return -1; // error code.
						}
						else {	// var declaration part without initialation
							declaration_init = 0;
							strcpy(id, $1); 
							enter(variable_single);
							stack_top++;
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
						//printf("current ident type is: %d in ident_def part\n", cur_type);
						//printf("current expr type is: %d in ident_def part\n", $3);
						
						if (cur_type != $3) {
							yyerror("Mismatched in data type!\n", 1002);
						}
						declaration_init = 1;
						strcpy(id, $1); 
						if (const_label == 1) {	// const declaration part
							enter(constant_single);
							/* need classification for different data type */
						}
						else {	// var declaration part with initialation
							enter(variable_single);
							stack_top++;
							int id_addr = 0;
							enum data_type_enum type_tmp;
							for (int i = 1; i <= sym_table_tail; i++) {
								if (strcmp($1, table[i].name) == 0) {
									id_addr = table[i].adr;
									break;
								}
							}
							gen(sto, cur_type, (char*)id_addr);
						}	
					}
				;

type:				INTSYM {cur_type = INT_TYPE;}
				|	CHARSYM	{cur_type = CHAR_TYPE;}
				|	STRINGSYM {cur_type = STRING_TYPE;}
				|	BOOLSYM {
						cur_type = BOOL_TYPE; 
						//printf("BOOL symbol find!\n");
					}
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

expression_stat:	expression_stat COMMA expression SEMICOLON pop_stack
				|	expression SEMICOLON pop_stack
				;

expression:			var {
						/*
						int i, res = -1;
						for (i = 1; i <= sym_table_tail; ++i) {
							if (strcmp(table[i].name, cur_id) == 0) {
								res = table[i].adr;
								break;
							}
						}
						//printf("check point\n");
						if (res == -1) {
							yyerror("Undefined _single!\n");
						}
						else {
							becomes_adr = cur_adr;
						}*/
						//becomes_adr = cur_adr;
						//printf("cur pos is expression_var part, cur_id is %s with adr %d\n", cur_id, becomes_adr);
					}				
					BECOMES expression {
						cur_kind = table[$1].kind;
						cur_type = table[$1].type;
						if (cur_kind == constant_single) {
							yyerror("Illegal assignment on constant_single!\n", 1003);
						}
						//printf("current var type is: %d in expression part\n", cur_type);
						//printf("current expr type is: %d in expression part\n", $3);
						//printf("current data stack top is: %d\n", *(int*)s[stack_top].val);
						//if ($1 != $4) {
						//	yyerror("Mismatched in data type!\n");
						//}
						becomes_adr = table[$1].adr;
						//printf("check point1 in expression\n");
						//printf("cur pos is expression part, cur_adr to sto is: %d\n", becomes_adr);
						if (table[$1].array_lable) {
							gen(stob, cur_type, (char*)becomes_adr);
						}
						else {
							gen(sto, cur_type, (char*)becomes_adr);
						}
						//printf("check point2 in expression\n");
						$$ = 0; // undefined
					}
				|	simple_expr {
						$$ = $1;
					}
				|	{

					}
				;

pop_stack:			{
						int opr_tmp = POP_OPR;
						gen(opr, 0, (char*)opr_tmp);
					}
				;

var:				IDENT {
						strcpy(cur_id, $1);
						//printf("current ident is %s in single var.\n", cur_id);
						int i, exist_flag = 0;
						for (i = 1; i <= sym_table_tail; ++i) {
							if (strcmp(cur_id, table[i].name) == 0) {
								exist_flag = 1;
								cur_table_adr = i;
								cur_adr = table[i].adr;
								$$ = i;
								cur_kind = table[i].kind;
								cur_type = table[i].type;
								//printf("current pos is var part, cur_id is %s in %d with type %d\n", cur_id, cur_adr, cur_type);
								break;
							}
						}
						if (exist_flag == 0) {
							yyerror("Undefinded variable_single!\n", 1004);
						}
					}
				|	dimension_exp {
						$$ = $1.sym;
						/*memset(tmp_dim_recoder, 0, sizeof(tmp_dim_recoder));
						tmp_dim_idx = 0;
						strcpy(cur_id, $1);
						printf("current ident is %s in ndim_arr.\n", cur_id);
						int i, j, exist_flag = 0;
						int tmp_size = 1, adr_shift = 0; 
						for (i = 1; i <= sym_table_tail; ++i) {
							if (strcmp(cur_id, table[i].name) == 0) {
								exist_flag = table[i].array_lable;
								cur_table_adr = i;
								cur_adr = table[i].adr;
								//$$ = INT_TYPE;
								cur_kind = table[i].kind;
								cur_type = table[i].type;
								/*
								printf("current dim_size arr is: ");
								for (j = 0; j < MAX_ARRAY_DIM; ++j) {
									printf("%d\t", table[i].array_dim_size[j]);
								}
								
								printf("\n");
								//printf("current pos is var_ndim part, cur_id is %s in %d with type %d\n", cur_id, cur_adr, cur_type);
								break;
							}
						}
						if (exist_flag == 0) {
							yyerror("Undefinded variable or non-array variable!\n");
						}
						ndim_var_lable = 1;
					} 
					dimension_exp RBRACK {
						$$ = cur_type;
					}*/
					}
				;

dimension_exp:		IDENT LBRACK expression RBRACK {
						strcpy(cur_id, $1);
						int i, exist_flag = 0;
						for (i = 1; i <= sym_table_tail; ++i) {
							if (strcmp(cur_id, table[i].name) == 0) {
								exist_flag = table[i].array_lable;
								break;
							}
						}
						if (!exist_flag) {
							yyerror("Undefinded variable or non-array variable!\n", 1005);
						}
						$$.sym = i;
						$$.dim = 1;						
					}
				|	dimension_exp {
						/*int j;
						printf("current dim_size arr is: ");
						for (j = 0; j < MAX_ARRAY_DIM; ++j) {
							printf("%d ", table[cur_table_adr].array_dim_size[j]);
						}
						printf("\n");*/
						//printf("gen dim_exp lit when tmp_idx is %d and val is %d\n", tmp_dim_idx, table[cur_table_adr].array_dim_size[tmp_dim_idx]);
						gen(lit, INT_TYPE, (char*)&(table[$1.sym].array_dim_size[$1.dim]));
						gen(opr, 0, TIMES_OPR);
					}
					LBRACK expression RBRACK {
						gen(opr, 0, PLUS_OPR);
						$$.sym = $1.sym;
						$$.dim = $1.dim+1;
						/*
						cur_table_adr = $$.sym;
						cur_adr = table[$$.sym].adr;
						cur_kind = table[$$.sym].kind;
						cur_type = table[$$.sym].type;
						*/
					}
				;

select_stat:		if_stat
				|	switch_stat
				;

if_stat:			IFSYM LPAREN expression RPAREN gen_jpc 
					statement	%prec LOWER_THAN_ELSE {
						int back_patch_pos = $5, back_patch_val = VM_pointer;
						memcpy((void*)code[back_patch_pos].val, (const void*)&back_patch_val, MAX_VAL_LEN);
					}
				|	IFSYM LPAREN expression RPAREN gen_jpc
					statement ELSESYM gen_jmp 
					statement {
						int back_patch_pos_pre = $5, back_patch_val_pre = $8+1;
						memcpy((void*)code[back_patch_pos_pre].val, (const void*)&back_patch_val_pre, MAX_VAL_LEN); // for false stat to jump in

						int back_patch_pos = $8, back_patch_val = VM_pointer;
						memcpy((void*)code[back_patch_pos].val, (const void*)&back_patch_val, MAX_VAL_LEN); // for ture stat to jump out
					}
				;

switch_stat:		SWITCHSYM LPAREN expression RPAREN LBPAREN case_list default_stat RBPAREN {
						gen(opr, 0, POP_OPR)
					}
				;

case_list:			case_list case_stat {
						//printf("current case_begin is: %d, case_end is %d\n", $2.case_begin, $2.case_end);
						int adr_tmp = $2.case_end;
						memcpy((void*)code[$2.case_begin].val, (const void*)&adr_tmp, MAX_VAL_LEN);
					}
				|	case_stat {
						//printf("current case_begin is: %d, case_end is %d\n", $1.case_begin, $1.case_end);
						int adr_tmp = $1.case_end;
						memcpy((void*)code[$1.case_begin].val, (const void*)&adr_tmp, MAX_VAL_LEN);
					}
				;

case_stat:			CASESYM	expression {
						int opr_tmp;
							opr_tmp = CASE_EQL_OPR;
							gen(opr, 0, (char*)opr_tmp);
							$1 = VM_pointer;
							opr_tmp = 0;
							gen(jpc, 0, (char*)opr_tmp);
						} 
					COLON compound_stat {
						$$.case_begin = $1;
						$$.case_end = VM_pointer;
					}
				;

default_stat:		DEFAULTSYM COLON compound_stat
				;

gen_jpc: 			{
						$$ = VM_pointer;
						int adr_tmp = 0;
						gen(jpc,0, (char*)adr_tmp);
					}
				;
gen_jmp:			{
						$$ = VM_pointer;
						int adr_tmp = 0;
						gen(jmp,0, (char*)adr_tmp);
					}
				;

iteration_stat:		for_stat
				|	while_stat
				|	do_while_stat
				|	repeat_stat
				;

for_stat:			FORSYM LPAREN expression SEMICOLON get_code_addr
					expression SEMICOLON get_code_addr {
						int opr_tmp = 0;
						gen(jpc, 0, (char*)opr_tmp); // false --- jump out 
						gen(jmp, 0, (char*)opr_tmp); // true --- jump to statement
					} 
					get_code_addr expression RPAREN {
						int adr_tmp = $5;
						gen(jmp, 0, (char*)adr_tmp); // back to condition part
					}
					get_code_addr
					statement {
						int adr_tmp = $10, adr_jpc_e2, adr_jpc_e2_pos = $8, adr_jmp_e2 = $14;
						gen(jmp, 0, (char*)adr_tmp);
						adr_jpc_e2 = VM_pointer;
						memcpy((void*)code[adr_jpc_e2_pos].val, (const void*)&adr_jpc_e2, MAX_VAL_LEN);
						memcpy((void*)code[adr_jpc_e2_pos+1].val, (const void*)&adr_jmp_e2, MAX_VAL_LEN);
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

repeat_stat:		REPEATSYM get_code_addr statement UNTILSYM LPAREN expression RPAREN SEMICOLON {
						gen(jpc, 0, $2);
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
						memcpy((void*)code[VM_pointer - 2].val, (const void*)&VM_pointer, MAX_VAL_LEN); // complete jump out adr
					}
				;

read_stat:			READSYM LPAREN read_list RPAREN SEMICOLON
				;

read_list:	 		read_list COMMA read_var
				|	read_var
				;

read_var:			var {
						if (cur_kind == constant_single || cur_kind == constant_array) {
							yyerror("Operation on contant data!\n", 1006);
						}
						gen(opr, cur_type, (char*)READ_OPR); // OPR undefined;
						cur_adr = table[$1].adr;
						if (table[$1].array_lable) {
							gen(stob, cur_type, (char*)cur_adr);
							int opr_tmp = POP_OPR;
							gen(opr, 0, (char*)opr_tmp);
						}
						else {
							gen(sto, cur_type, (char*)cur_adr);
							int opr_tmp = POP_OPR;
							gen(opr, 0, (char*)opr_tmp);
						}
					}
				;

write_stat:			WRITESYM LPAREN write_list RPAREN SEMICOLON
				;

write_list:			write_list COMMA expression {
						int opr_tmp = WRITE_OPR;
						gen(opr, 0, (char*)opr_tmp);
					}
				|	expression {
						int opr_tmp = WRITE_OPR;
						gen(opr, 0, (char*)opr_tmp);
					}
				;

simple_expr: 		additive_expr {
						$$ = $1;
					}
				|	simple_expr binary_bool_opr additive_expr {
						$$ = BOOL_TYPE;
						int opr_tmp = $2;
						gen(opr, 0, (char*)opr_tmp);
					}
				;

additive_expr:		additive_expr binary_add_opr multiplying_expr {
						$$ = $1;
						int opr_tmp = $2;
						gen(opr, 0, (char*)opr_tmp);
					}
				|	multiplying_expr {
						$$ = $1;
					}
				;

multiplying_expr:	multiplying_expr binary_mul_opr term {
						$$ = $1;
						int opr_tmp = $2;
						gen(opr, 0, (char*)opr_tmp);
					}
				|	term {
						$$ = $1;
					}
				;

term:				factor {
						$$ = $1;
					}
				|	unary_opr term {
						$$ = $2;
						int opr_tmp = $1 + 3;
						printf("current opr in unary_opr term is: %d\n", opr_tmp);
						//opr_tmp = (opr_tmp == INC_OPR ? PLUS_OPR : MINUS_OPR);
						int tmp_num = 1;
						gen(lit, INT_TYPE, (char*)&tmp_num);
						gen(opr, 0, (char*)opr_tmp);
						if (table[cur_table_adr].array_lable) {
							gen(stob, cur_type, (char*)cur_adr);
							gen(lit, INT_TYPE, (char*)&cur_table_adr);
							gen(lodb, cur_type, (char*)cur_adr);
						}
						else {
							gen(sto, cur_type, (char*)cur_adr);
							gen(lod, cur_type, (char*)cur_adr);
						}
						//gen(lit, 0, (char*)tmp_num);
					}
				|	MINUS factor %prec UMINUS {
						$$ = $2;
						int opr_tmp = UMINUS_OPR;
						gen(opr, 0, (char*)opr_tmp);
					}
				|	NOT factor {
						$$ = BOOL_TYPE;
						int opr_tmp = NOT_OPR;
						gen(opr, 0, (char*)opr_tmp);
					}
				|	ODDSYM factor {
						$$ = BOOL_TYPE;
						int opr_tmp = ODD_OPR;
						gen(opr, 0, (char*)opr_tmp);
					}
				;

factor:				NUMBER {
						$$ = INT_TYPE;
						input_val_int = $1;
						//printf("factor part input val(int) is: %d\n", input_val_int);
						gen(lit, INT_TYPE, (char*)&input_val_int);
					}
				|	BOOL {
						$$ = BOOL_TYPE;
						input_val_bool = $1;
						//printf("factor part input val(bool) is: %d\n", input_val_bool);
						gen(lit, BOOL_TYPE, (char*)&input_val_bool);
					}
				|	STRING {
						$$ = STRING_TYPE;
						strcpy(input_val_string, $1);
						//input_val_string[strlen(input_val_string)-1] = '\0';
						//printf("factor part input val(string) is: %s\n", input_val_string);
						gen(lit, STRING_TYPE, &input_val_string);
					}
				|	CHAR {
						$$ = CHAR_TYPE;
						input_val_char = $1;
						//printf("factor part input val(char) is: %c\n", input_val_char);
						gen(lit, CHAR_TYPE, (char*)&input_val_char);
					}
				| 	var {
						cur_table_adr = $1;
						cur_adr = table[$1].adr;
						cur_kind = table[$1].kind;
						if (cur_kind == constant_single) {
							gen(lit, cur_type, table[cur_table_adr].val);
						}
						else if (cur_kind == variable_single){
							// Maybe refresh the data_stack val is required.
							gen(lod, cur_type, (char*)cur_adr);
						}
						else {
							gen(lit, INT_TYPE, (char*)&cur_table_adr);
							gen(lodb, cur_type, (char*)cur_adr);
						}
						ndim_var_lable = 0;
					}
				|	LPAREN expression RPAREN {
						$$ = $2;
					}
				;



binary_add_opr:		PLUS {
						$$ = PLUS_OPR;
					}
				|	MINUS {
						$$ = MINUS_OPR;
					}
				;

binary_mul_opr:		TIMES {
						$$ = TIMES_OPR;
					}
				|	SLASH {
						$$ = SLASH_OPR;
					}
				|	MOD {
						$$ = MOD_OPR;
					}
				;

binary_bool_opr:	AND {	
						$$ = AND_OPR;
					}
				|	OR {
						$$ = OR_OPR;
					}
				|	XOR {
						$$ = XOR_OPR;
					}
				|	LSS {
						$$ = LSS_OPR;
					}
				|	LEQ {
						$$ = LEQ_OPR;
					}
				|	GTR {
						$$ = GTR_OPR;
					}
				|	GEQ {
						$$ = GEQ_OPR;
					}
				|	EQL {
						$$ = EQL_OPR;
					}
				|	NEQ {
						$$ = NEQ_OPR;
					}
				;

unary_opr:			INC {
						$$ = INC_OPR;
					}
				|	DEC {
						$$ = DEC_OPR;
					}
				;

////////////////////////////////////////////////////////
//程序部分
%%
int yyerror(char *s, int error_num) {
	err = err + 1;
  	printf("%d: %s in line %d\n", error_num, s, line);
	fprintf(foutput, "%d: %s in line %d\n", error_num, s, line);
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
	int i;
	char data_type_name[][15]= {
		{"integer"},{"boolean"},{"string_chars"},{"single_char"},
	};

	sym_table_tail++;
	strcpy(table[sym_table_tail].name, id);
	table[sym_table_tail].kind = k;
	table[sym_table_tail].init_lable = declaration_init;
	table[sym_table_tail].type = cur_type;
	
	//printf("current pos is enter part, the input id is: %s with declaration_init: %d, cur_type: %d\n", id, declaration_init, cur_type);
	

	//printf("%d:\t%d\t%s\t%d\t%d\n", sym_table_tail, k_kind, id, k_type, declaration_init);
	
	switch(k) {
		case constant_single:
			switch (cur_type) {
				case INT_TYPE:
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_int, MAX_VAL_LEN);
					//table[sym_table_tail].adr = curr_address++;
					break;
				case BOOL_TYPE:
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_bool, MAX_VAL_LEN);
					//table[sym_table_tail].adr = curr_address++;
					break;
				case CHAR_TYPE:
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_char, MAX_VAL_LEN);
					//table[sym_table_tail].adr = curr_address++;
					break;
				case STRING_TYPE:
					memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_string, MAX_VAL_LEN);
					//table[sym_table_tail].adr = curr_address++;
					break;
				default:
					printf("Unknown data type in constant_single enter part!");
			}
			break;
		case constant_array:
			table[sym_table_tail].array_lable = 1;
			break;
		case variable_single:
			//printf("variable_single entered!\n");
			switch (cur_type) {
				case INT_TYPE:
					if (declaration_init) {
						memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_int, MAX_VAL_LEN);
					}
					table[sym_table_tail].adr = curr_address++;
					break;
				case BOOL_TYPE:
					if (declaration_init) {
						memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_bool, MAX_VAL_LEN);
					}
					table[sym_table_tail].adr = curr_address++;
					break;
				case CHAR_TYPE:
					if (declaration_init) {
						memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_char, MAX_VAL_LEN);
					}
					table[sym_table_tail].adr = curr_address++;
					break;
				case STRING_TYPE:
					if (declaration_init) {
						memcpy((void*)&table[sym_table_tail].val, (const void*)&input_val_string, MAX_VAL_LEN);
					}
					table[sym_table_tail].adr = curr_address++;
					break;
				default:
					printf("Unknown data type in variable_single enter part!");
			}
			break;
		case variable_array:
			table[sym_table_tail].array_lable = 1;
			table[sym_table_tail].adr = curr_address;
			table[sym_table_tail].size = arr_size;
			for (i = 0; i < MAX_ARRAY_DIM; ++i) {
				table[sym_table_tail].array_dim_size[i] = arr_dim_recoder[i];
			}
			curr_address += arr_size;
			printf("current vatiable defination is a n_dim variable with length %d\n", arr_size);
			break;
		default:
			yyerror("Unknown kind occured in enter!\n", 1007);
	}
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
void gen(enum fct x, int y, char z[MAX_VAL_LEN]) {
	//printf("check point1 in gen part\n");
	int fct_num;
	char name[][5]= {
		{"lit"},{"opr"},{"lod"},{"sto"},{"lodb"},{"stob"},{"int"},{"jmp"},{"jpc"},
	};
	//printf("fct is: %s\ty is: %d\t", name[x], y);
	/*switch (cur_type) {
		case INT_TYPE:
			printf("z(int) is: %d\n", *(int*)&z);
			break;
		case BOOL_TYPE:
			printf("z(bool) is: ");
			printf("%d\n", *(int*)&z);
			break;
		case CHAR_TYPE:
			printf("z(char) is: %c\n", *(char*)&z);
			break;
		case STRING_TYPE:
			printf("z(string) is: ");
			printf("%s\n", (char*)&z);
			break;
		default:
			printf("fct(default) is: %d\n", 114514);
			break;
	}*/
	//printf("cur id is: %s\tcur adr is: %d\t", cur_id, cur_adr);
	//printf("cur val is: %d\n", *(int*)&table[cur_adr].val);

	if (VM_pointer >= MAX_CODE) {
		printf("Program is too long!\n");	/* 生成的虚拟机代码程序过长 */
		exit(1);
	}
	code[VM_pointer].f = x;
	code[VM_pointer].l = y;
	if (x == lit) {
		//printf("check point 1 in gen part\n");
		memcpy((void*)(code[VM_pointer].val), (const void*)z, MAX_VAL_LEN); // lit has different behavior with other instruction, causion.
		//printf("check point 2 in gen part\n");
	}
	else {
		//printf("check point 3 in gen part\n");
		memcpy((void*)(code[VM_pointer].val), (const void*)&z, MAX_VAL_LEN);
		//printf("check point 4 in gen part\n");
	}
	VM_pointer++;
}

/* 输出所有目标代码  */
void listall() {
	printf("==================list all=========================\n");
	int i;
	char name[][5]= {
		{"lit"},{"opr"},{"lod"},{"sto"},{"lodb"},{"stob"},{"int"},{"jmp"},{"jpc"},
	};
	if (listall_switch_on) {
		for (i = 0; i < VM_pointer; i++) {
			if (code[i].f == lit){
				switch (code[i].l) {
					case INT_TYPE:
						printf("%d %s %d %d\n", i, name[code[i].f], code[i].l, *(int*)&code[i].val);
						fprintf(fcode,"%d %s %d %d\n", i, name[code[i].f], code[i].l, *(int*)&code[i].val);
						break;
					case BOOL_TYPE:
						printf("%d %s %d %s\n", i, name[code[i].f], code[i].l, *(int*)(&code[i].val) == 1 ? "true" : "false");
						fprintf(fcode,"%d %s %d %s\n", i, name[code[i].f], code[i].l, *(int*)&code[i].val == 1 ? "true" : "false");
						break;
					case CHAR_TYPE: // test required.
						printf("%d %s %d %c\n", i, name[code[i].f], code[i].l, *(char*)&code[i].val);
						fprintf(fcode,"%d %s %d %c\n", i, name[code[i].f], code[i].l, *(char*)&code[i].val);
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
	printf("==================list all=========================\n");
}

/* 输出符号表 */
void displaytable() {
	printf("==================display table=========================\n");
	int i;
	printf(" idx\tkind\ttype\tname\tval\tadr\n");
	fprintf(ftable, " idx\tkind\ttype\tname\tval\tadr\n");
	if (sym_table_switch_on) {		/* 输出符号表 */
	
		for (i = 1; i <= sym_table_tail; ++i) {
			switch (table[i].kind) {
				case constant_single:
					switch (table[i].type) {
						case INT_TYPE:
							printf("%4d\tconst\tint\t%20s\t%d\t%d\n", i, table[i].name, *(int*)&table[i].val, table[i].adr);
							fprintf(ftable, "%4d\tconst\tint\t%20s\t%d\t%d\n", i, table[i].name, *(int*)&table[i].val, table[i].adr);
							break;
						case BOOL_TYPE:
							printf("%4d\tconst\tbool\t%20s\t%s\t%d\n", i, table[i].name, *(int*)&table[i].val == 1 ? "true" : "false", table[i].adr);
							fprintf(ftable, "%4d\tconst\tbool\t%20s\t%s\t%d\n", i, table[i].name, *(int*)&table[i].val == 1 ? "true" : "false", table[i].adr);
							break;
						case CHAR_TYPE:
							printf("%4d\tconst\tchar\t%20s\t%c\t%d\n", i, table[i].name, *(char*)&table[i].val, table[i].adr);
							fprintf(ftable, "%4d\tconst\tchar\t%20s\t%c\t%d\n", i, table[i].name, *(char*)&table[i].val, table[i].adr);
							break;
						case STRING_TYPE:
							printf("%4d\tconst\tstring\t%20s\t%s\t%d\n", i, table[i].name, table[i].val, table[i].adr);
							fprintf(ftable, "%4d\tconst\tstring\t%20s\t%s\t%d\n", i, table[i].name, table[i].val, table[i].adr);
							break;
					}
					break;
				case variable_single:
					switch (table[i].type) {
						case INT_TYPE:
							printf("%4d\tvar\tint\t%20s\t%d\t%d\n", i, table[i].name, *(int*)&table[i].val, table[i].adr);
							fprintf(ftable, "%4d\tvar\tint\t%20s\t%d\t%d\n", i, table[i].name, *(int*)&table[i].val, table[i].adr);
							break;
						case BOOL_TYPE:
							printf("%4d\tvar\tbool\t%20s\t%s\t%d\n", i, table[i].name, *(int*)&table[i].val == 1 ? "true" : "false", table[i].adr);
							fprintf(ftable, "%4d\tvar\tbool\t%20s\t%s\t%d\n", i, table[i].name, *(int*)&table[i].val == 1 ? "true" : "false", table[i].adr);
							break;
						case CHAR_TYPE:
							printf("%4d\tvar\tchar\t%20s\t%c\t%d\n", i, table[i].name, *(char*)&table[i].val, table[i].adr);
							fprintf(ftable, "%4d\tvar\tchar\t%20s\t%c\t%d\n", i, table[i].name, *(char*)&table[i].val, table[i].adr);
							break;
						case STRING_TYPE:
							printf("%4d\tvar\tstring\t%20s\t%s\t%d\n", i, table[i].name, table[i].val, table[i].adr);
							fprintf(ftable, "%4d\tvar\tstring\t%20s\t%s\t%d\n", i, table[i].name, table[i].val, table[i].adr);
							break;
					}
					break;
				case variable_array:
					switch (table[i].type) {
						case INT_TYPE:
							printf("%4d\tvar\tint\t%20s\t%d\t%d\n", i, table[i].name, table[i].size, table[i].adr);
							fprintf(ftable, "%4d\tvar\tint\t%20s\t%d\t%d\n", i, table[i].name, table[i].size, table[i].adr);
							break;
						case BOOL_TYPE:
							printf("%4d\tvar\tbool\t%20s\t%s\t%d\n", i, table[i].name, table[i].size , table[i].adr);
							fprintf(ftable, "%4d\tvar\tbool\t%20s\t%d\t%d\n", i, table[i].name, table[i].size, table[i].adr);
							break;
						case CHAR_TYPE:
							printf("%4d\tvar\tchar\t%20s\t%c\t%d\n", i, table[i].name, table[i].size, table[i].adr);
							fprintf(ftable, "%4d\tvar\tchar\t%20s\t%d\t%d\n", i, table[i].name, table[i].size, table[i].adr);
							break;
						case STRING_TYPE:
							printf("%4d\tvar\tstring\t%20s\t%s\t%d\n", i, table[i].name, table[i].size, table[i].adr);
							fprintf(ftable, "%4d\tvar\tstring\t%20s\t%d\t%d\n", i, table[i].name, table[i].size, table[i].adr);
							break;
					}
					break;
			}
		}
		printf("\n");
		fprintf(ftable, "\n");
	}
	printf("==================display table=========================\n");

}


/* 解释程序 */
void interpret() {
	int pc = 0; /* 指令指针 */
	struct instruction i;
	int loop_i;
	int lod_table_idx;
	int opr_tmp_res;
	int opr_tmp_l, opr_tmp_r;
	char opr_tmp_char_l, opr_tmp_char_r;
	enum data_type_enum type_tmp_l;
	enum data_type_enum type_tmp_r;
	char opr_tmp_string[MAX_VAL_LEN];
	int lod_table_adr, sto_table_adr;
	
	char name[][5]= {
		{"lit"},{"opr"},{"lod"},{"sto"},{"lodb"},{"stob"},{"int"},{"jmp"},{"jpc"},
	};
	char type_name[][15]= {
		{"integer"},{"boolean"},{"string_chars"},{"single_char"},
	};	

	printf("Start X0Compiler.\n");
	fprintf(fresult, "Start X0Compiler.\n");
	
	do {
		i = code[pc];
		printf("======interpret part in code[%d]======\n", pc);
		printf("%s\t%d\t%d\n", name[i.f], i.l, *(int*)i.val); 
		//printf("current pos 5 is: %d\n", *(int*)&s[5].val);
		pc++;
		switch (i.f) {
			case lit:
				stack_top++;
				memcpy((void*)(&(s[stack_top].val)), (const void*)(&i.val), MAX_VAL_LEN);
				switch(i.l) {
					case INT_TYPE:
						s[stack_top].t = integer;
						//printf("lit type is: %s\t", type_name[s[stack_top].t]);
						//printf("lit part(int) stack_top is: %d\n", *(int*)s[stack_top].val);
						break;
					case BOOL_TYPE:
						s[stack_top].t = boolean;
						//printf("lit part(bool) stack_top is: %d\n", *(int*)s[stack_top].val);
						break;
					case CHAR_TYPE:
						s[stack_top].t = single_char;
						//printf("lit part(char) stack_top is: %c\n", *(char*)s[stack_top].val);
						break;
					case STRING_TYPE:
						s[stack_top].t = string_chars;
						//printf("lit part(string) stack_top is: %s\n", s[stack_top].val);
						break;
				}
				break;
			case opr:
				switch (*(int*)i.val) {
					case INC_OPR:
						if (s[stack_top].t == integer) {
							opr_tmp_res = *(int*)&s[stack_top].val + 1;
							memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_res, MAX_VAL_LEN);
						}
						else {
							yyerror("Integer required in INC!\n", 1008);
						}
						break;

					case DEC_OPR:
						if (s[stack_top].t == integer) {
							opr_tmp_res = *(int*)&s[stack_top].val - 1;
							memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_res, MAX_VAL_LEN);
						}
						else {
							yyerror("Integer required in DEC!\n", 1009);
						}
						break;

					case NOT_OPR:
						opr_tmp_res = *(int*)&s[stack_top].val ? 0 : 1;
						memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_res, MAX_VAL_LEN);
						s[stack_top].t = boolean;
						break;

					case PLUS_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top-1].t;
						if (type_tmp_l != type_tmp_r){
							printf("Warning: Data type mismatched in PLUS!\n");
						}
						opr_tmp_l = *(int*)&s[stack_top].val;
						opr_tmp_r = *(int*)&s[--stack_top].val;
						opr_tmp_r += opr_tmp_l;
						memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_r, MAX_VAL_LEN);
						s[stack_top].t = type_tmp_r;
						break;

					case MINUS_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top-1].t;
						if (type_tmp_l != type_tmp_r){
							printf("Warning: Data type mismatched in MINUS!\n");
						}
						opr_tmp_l = *(int*)&s[stack_top].val;
						opr_tmp_r = *(int*)&s[--stack_top].val;
						opr_tmp_l = opr_tmp_r - opr_tmp_l;
						memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						s[stack_top].t = type_tmp_r;
						break;

					case TIMES_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top-1].t;
						if ((type_tmp_l != type_tmp_r) || (type_tmp_l != integer)){
							yyerror("Data type mismatched in TIMES!\n", 1010);
						}
						else {
							opr_tmp_l = *(int*)&s[stack_top].val;
							opr_tmp_r = *(int*)&s[--stack_top].val;
							//printf("current opr is *, the l val is %d, the r val is %d, the res is %d\n", opr_tmp_l, opr_tmp_r, opr_tmp_l*opr_tmp_r);
							opr_tmp_l *= opr_tmp_r;
							memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						}
						break;
					
					case SLASH_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top-1].t;
						if ((type_tmp_l != type_tmp_r) || (type_tmp_l != integer)){
							yyerror("Data type mismatched in SLASH!\n", 1011);
						}
						else {
							opr_tmp_l = *(int*)&s[stack_top].val;
							opr_tmp_r = *(int*)&s[--stack_top].val;
							opr_tmp_l = opr_tmp_r / opr_tmp_l;
							memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						}
						break;

					case MOD_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top-1].t;
						if ((type_tmp_l != type_tmp_r) || (type_tmp_l != integer)){
							yyerror("Data type mismatched in MOD!\n", 1012);
						}
						else {
							opr_tmp_l = *(int*)&s[stack_top].val;
							opr_tmp_r = *(int*)&s[--stack_top].val;
							opr_tmp_l = opr_tmp_r % opr_tmp_l;
							memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						}
						break;

					case AND_OPR:
						type_tmp_l = s[stack_top].t;
						switch(type_tmp_l) {
							case integer:
								opr_tmp_l = *(int*)&s[stack_top].val;
								break;
							case boolean:
								opr_tmp_l = *(bool*)&s[stack_top].val;
								break;
							default:
								yyerror("Data type mismatched in AND!\n", 1013);
								break;
						}
						type_tmp_r = s[stack_top - 1].t;
						switch(type_tmp_r) {
							case integer:
								opr_tmp_r = *(int*)&s[stack_top - 1].val;
								break;
							case boolean:
								opr_tmp_r = *(bool*)&s[stack_top - 1].val;
								break;
							default:
								yyerror("Data type mismatched in AND!\n", 1013);
								break;
						}
						opr_tmp_l = opr_tmp_l && opr_tmp_r;
						memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						s[stack_top].t = boolean;
						break;

					case OR_OPR:
						type_tmp_l = s[stack_top].t;
						switch(type_tmp_l) {
							case integer:
								opr_tmp_l = *(int*)&s[stack_top].val;
								break;
							case boolean:
								opr_tmp_l = *(bool*)&s[stack_top].val;
								break;
							default:
								yyerror("Data type mismatched in OR!\n", 1014);
								break;
						}
						type_tmp_r = s[stack_top - 1].t;
						switch(type_tmp_r) {
							case integer:
								opr_tmp_r = *(int*)&s[stack_top - 1].val;
								break;
							case boolean:
								opr_tmp_r = *(bool*)&s[stack_top - 1].val;
								break;
							default:
								yyerror("Data type mismatched in OR!\n", 1014);
								break;
						}
						opr_tmp_l = opr_tmp_l || opr_tmp_r;
						memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						s[stack_top].t = boolean;
						break;

					case XOR_OPR:
						type_tmp_l = s[stack_top].t;
						switch(type_tmp_l) {
							case integer:
								opr_tmp_l = *(int*)&s[stack_top].val;
								break;
							case boolean:
								opr_tmp_l = *(bool*)&s[stack_top].val;
								break;
							default:
								yyerror("Data type mismatched in XOR!\n", 1015);
								break;
						}
						type_tmp_r = s[stack_top - 1].t;
						switch(type_tmp_r) {
							case integer:
								opr_tmp_r = *(int*)&s[stack_top - 1].val;
								break;
							case boolean:
								opr_tmp_r = *(bool*)&s[stack_top - 1].val;
								break;
							default:
								yyerror("Data type mismatched in XOR!\n", 1015);
								break;
						}
						if ((opr_tmp_l && opr_tmp_r) || (!opr_tmp_l && !opr_tmp_r)) {
							opr_tmp_l = 0;
						}
						else {
							opr_tmp_l = 1;
						}
						memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						s[stack_top].t = boolean;
						break;

					case LSS_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top - 1].t;
						if (type_tmp_l != type_tmp_r) {
							//yyerror("Missmatched in LSS part!\n");
						}
						else {
							switch (type_tmp_l) {
								case integer:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l < opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case boolean:
									yyerror("BOOL has no LSS opr!\n", 1016);
									break;
								case single_char:
									opr_tmp_char_l = *(char*)&s[stack_top - 1].val;
									opr_tmp_char_r = *(char*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_char_l < opr_tmp_char_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case string_chars:
									opr_tmp_l = strcmp(s[stack_top - 1].val, s[stack_top].val) < 0 ? 1 : 0;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
							}
						}
						break;

					case LEQ_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top - 1].t;
						if (type_tmp_l != type_tmp_r) {
							//yyerror("Missmatched in LEQ part!\n");
						}
						else {
							switch (type_tmp_l) {
								case integer:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l <= opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case boolean:
									yyerror("BOOL has no LEQ opr!\n", 1017);
									break;
								case single_char:
									opr_tmp_char_l = *(char*)&s[stack_top - 1].val;
									opr_tmp_char_r = *(char*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_char_l <= opr_tmp_char_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case string_chars:
									opr_tmp_l = strcmp(s[stack_top - 1].val, s[stack_top].val) <= 0 ? 1 : 0;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
							}
						}
						break;
					
					case GTR_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top - 1].t;
						if (type_tmp_l != type_tmp_r) {
							//yyerror("Missmatched in GTR part!\n");
						}
						else {
							switch (type_tmp_l) {
								case integer:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l > opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case boolean:
									yyerror("BOOL has no GTR opr!\n", 1018);
									break;
								case single_char:
									opr_tmp_char_l = *(char*)&s[stack_top - 1].val;
									opr_tmp_char_r = *(char*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_char_l > opr_tmp_char_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case string_chars:
									opr_tmp_l = strcmp(s[stack_top - 1].val, s[stack_top].val) > 0 ? 1 : 0;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
							}
						}
						break;

					case GEQ_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top - 1].t;
						if (type_tmp_l != type_tmp_r) {
							//yyerror("Missmatched in GEQ part!\n");
						}
						else {
							switch (type_tmp_l) {
								case integer:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l >= opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case boolean:
									yyerror("BOOL has no LSS opr!\n", 1019);
									break;
								case single_char:
									opr_tmp_char_l = *(char*)&s[stack_top - 1].val;
									opr_tmp_char_r = *(char*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_char_l >= opr_tmp_char_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case string_chars:
									opr_tmp_l = strcmp(s[stack_top - 1].val, s[stack_top].val) >= 0 ? 1 : 0;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
							}
						}
						break;

					case EQL_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top - 1].t;
						if (type_tmp_l != type_tmp_r) {
						//	yyerror("Missmatched in EQL part!\n");
						}
						else {
							switch (type_tmp_l) {
								case integer:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l == opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case boolean:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l == opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case single_char:
									opr_tmp_char_l = *(char*)&s[stack_top - 1].val;
									opr_tmp_char_r = *(char*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_char_l == opr_tmp_char_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case string_chars:
									opr_tmp_l = strcmp(s[stack_top - 1].val, s[stack_top].val) == 0 ? 1 : 0;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
							}
						}
						break;

					case NEQ_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top - 1].t;
						//printf("current pos bool opr(NEQ):\n");
						//printf("left type is: %s\t", type_name[type_tmp_r]);
						//printf("right type is: %s\n", type_name[type_tmp_l]);
						//printf("left val is: %d\t", *(int*)&s[stack_top - 1].val);
						//printf("right val is: %d\n", *(int*)&s[stack_top].val);
						if (type_tmp_l != type_tmp_r) {
						//	yyerror("Missmatched in NEQ part!\n");
						}
						else {
							switch (type_tmp_r) {
								case integer:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l != opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case boolean:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l != opr_tmp_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case single_char:
									opr_tmp_char_l = *(char*)&s[stack_top - 1].val;
									opr_tmp_char_r = *(char*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_char_l != opr_tmp_char_r;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case string_chars:
									opr_tmp_l = strcmp(s[stack_top - 1].val, s[stack_top].val) != 0 ? 1 : 0;
									memcpy((void*)s[--stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
							}
						}
						break;
					
					case READ_OPR:
						stack_top++;
						printf("stack_top pos in READ_OPR part is: %d and type is %d\n", stack_top, i.l);
						switch (i.l) {
							case INT_TYPE:
								scanf("%d", &opr_tmp_l);
								//input_val_int = opr_tmp_l;
								memcpy((void*)(&(s[stack_top].val)), (const void*)&opr_tmp_l, MAX_VAL_LEN);
								s[stack_top].t = integer;
								break;
							case BOOL_TYPE:
								scanf("%s", opr_tmp_string);
								fflush(stdin);
								for (loop_i = 0; loop_i < strlen(opr_tmp_string); ++loop_i) {
									opr_tmp_string[loop_i] = tolower(opr_tmp_string[loop_i]);
								}
								if (strcmp(opr_tmp_string, "true") == 0) {
									opr_tmp_l = 1;
								}
								else if (strcmp(opr_tmp_string, "false") == 0) {
									opr_tmp_l = 0;
								}
								else {
									yyerror("Illegal for read bool!\n", 1020);
								}
								//opr_tmp_l = strcmp(opr_tmp_string, "true") == 0 ? 1 : 0;
								//input_val_bool = opr_tmp_l;
								memcpy((void*)(&(s[stack_top].val)), (const void*)&opr_tmp_l, MAX_VAL_LEN);
								s[stack_top].t = boolean;
								break;
							case CHAR_TYPE:
								scanf("%c", &opr_tmp_char_l);
								fflush(stdin);
								//input_val_char = opr_tmp_char_l;
								memcpy((void*)(&(s[stack_top].val)), (const void*)&opr_tmp_char_l, MAX_VAL_LEN);
								s[stack_top].t = single_char;
								break;
							case STRING_TYPE:
								gets(opr_tmp_string);
								//getchar();
								printf("the input string is: %s with len is %d\n", opr_tmp_string, strlen(opr_tmp_string));
								//input_val_string = opr_tmp_string;
								memcpy((void*)(&(s[stack_top].val)), (const void*)&opr_tmp_string, MAX_VAL_LEN);
								s[stack_top].t = string_chars;
								break;
						}
						break;
					case WRITE_OPR:
						printf("current pos is write opr: %s\n", type_name[s[stack_top].t]);
						switch (s[stack_top].t) {
							case integer:
								printf("%d\n", *(int*)&s[stack_top].val);
								fprintf(fresult, "%d\n", *(int*)&s[stack_top--].val);
								break;
							case boolean:
								printf("%s\n", (*(int*)&s[stack_top].val == 1) ? "true" : "false");
								fprintf(fresult, "%s\n", (*(int*)&s[stack_top--].val == 1) ? "true" : "false");
								break;
							case single_char:
								printf("%c\n", *(char*)&s[stack_top].val);
								fprintf(fresult, "%c\n", *(char*)&s[stack_top--].val);
								break;
							case string_chars:
								printf("%s\n", s[stack_top].val);
								fprintf(fresult, "%s\n", s[stack_top--].val);
								break;
						}
						break;
					case UMINUS_OPR:
						type_tmp_l = s[stack_top].t;
						if (type_tmp_l != integer){
							yyerror("Data type mismatched in MINUS!\n", 1020);
						}
						else {
							opr_tmp_l = *(int*)&s[stack_top].val;
							opr_tmp_l = -opr_tmp_l;
							memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						}
						break;
					case ODD_OPR:
						type_tmp_l = s[stack_top].t;
						if (type_tmp_l != integer) {
							yyerror("ODD_OPR only support integer type!\n", 1020);
						}
						opr_tmp_l = *(int*)*&s[stack_top].val;
						opr_tmp_l %= 2;
						memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
						s[stack_top].t = boolean;
						break;
					case POP_OPR:
						stack_top--;
						break;
					/*case OFF_OPR:
						int dim_i, tmp_val, offset;
						int table_idx = i.l;
						int tmp_size = 1, adr_shift = 0; 
						for (dim_i = tmp_dim_idx-1; dim_i >= 0; --dim_i) {
							if (tmp_dim_recoder[dim_i].idx_type == 0) {	// num
								tmp_val = tmp_dim_recoder[dim_i].val;
							}
							else {	// val
								tmp_val = *(int*)&s[tmp_dim_recoder[dim_i].val].val;
							}
							adr_shift += (tmp_val * tmp_size);
							tmp_size *= arr_dim_recoder[dim_i];
						}
						if (adr_shift > table[table_idx].size) {
							yyerror("ndim var out of range!\n");
						}
						else {
							offset = adr_shift + table[]
						}*/
					case CASE_EQL_OPR:
						type_tmp_l = s[stack_top].t;
						type_tmp_r = s[stack_top - 1].t;
						if (type_tmp_l != type_tmp_r) {
						//	yyerror("Missmatched in EQL part!\n");
						}
						else {
							switch (type_tmp_l) {
								case integer:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l == opr_tmp_r;
									memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case boolean:
									opr_tmp_l = *(int*)&s[stack_top - 1].val;
									opr_tmp_r = *(int*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_l == opr_tmp_r;
									memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case single_char:
									opr_tmp_char_l = *(char*)&s[stack_top - 1].val;
									opr_tmp_char_r = *(char*)&s[stack_top].val;
									opr_tmp_l = opr_tmp_char_l == opr_tmp_char_r;
									memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
								case string_chars:
									opr_tmp_l = strcmp(s[stack_top - 1].val, s[stack_top].val) == 0 ? 1 : 0;
									memcpy((void*)s[stack_top].val, (const void*)&opr_tmp_l, MAX_VAL_LEN);
									s[stack_top].t = boolean;
									break;
							}
						}
						break;

					default:
						yyerror("Undefined opr occured!\n", 1022);
				}
				break;
			case lod:
				stack_top++;
				lod_table_adr = *(int*)&(i.val);
				memcpy((void*)(&(s[stack_top].val)), (const void*)(&(s[lod_table_adr].val)), MAX_VAL_LEN);
				s[stack_top].t = s[lod_table_adr].t;
				break;
			case sto:
				sto_table_adr = *(int*)&(i.val);
				//printf("sto_opr_check\n");
				//printf("current stack top in sto is: %d\n", stack_top);
				
				memcpy((void*)(&(s[sto_table_adr].val)), (const void*)(&(s[stack_top].val)), MAX_VAL_LEN);
				
				switch(i.l) {
					case INT_TYPE:
						s[sto_table_adr].t = integer;
						break;
					case BOOL_TYPE:
						s[sto_table_adr].t = boolean;
						break;
					case CHAR_TYPE:
						s[sto_table_adr].t = single_char;
						break;
					case STRING_TYPE:
						s[sto_table_adr].t = string_chars;
						break;
				}
				break;
			case lodb:
				lod_table_idx = *(int*)&s[stack_top--].val;
				if (table[lod_table_idx].size <= *(int*)&s[stack_top].val) {
					yyerror("ndim_array idx out of range!\n", 1021);
				}
				//stack_top++;
				lod_table_adr = *(int*)&(i.val) + *(int*)&s[stack_top].val;
				//printf("current lodb address is %d\n", lod_table_adr);
				//printf("the lod adr value is: %d\n", *(int*)&(s[lod_table_adr].val));
				//printf("before lodb value is %d\n", *(int*)&(s[stack_top].val));
				memcpy((void*)(&(s[stack_top].val)), (const void*)(&(s[lod_table_adr].val)), MAX_VAL_LEN);
				//printf("after lodb value is %d\n", *(int*)&(s[stack_top].val));
				s[stack_top].t = s[lod_table_adr].t;
				break;
			case stob:
				sto_table_adr = *(int*)&(i.val);
				sto_table_adr += *(int*)&(s[stack_top-1].val);
				//printf("current stob address is %d\n", sto_table_adr);
				//printf("current data_stack top is: %d, next is %d\n", *(int*)&s[stack_top].val, *(int*)&s[stack_top-1].val);
				memcpy((void*)(&(s[sto_table_adr].val)), (const void*)(&(s[stack_top].val)), MAX_VAL_LEN);
				memcpy((void*)(&(s[stack_top-1].val)), (const void*)(&(s[stack_top].val)), MAX_VAL_LEN);
				
				//printf("current value in stob pos is %d\n", *(int*)&s[sto_table_adr].val);
				switch(i.l) {
					case INT_TYPE:
						s[sto_table_adr].t = integer;
						break;
					case BOOL_TYPE:
						s[sto_table_adr].t = boolean;
						break;
					case CHAR_TYPE:
						s[sto_table_adr].t = single_char;
						break;
					case STRING_TYPE:
						s[sto_table_adr].t = string_chars;
						break;
				}
				stack_top--;
				break;
			
			case ini:
				stack_top = stack_top + *(int*)&i.val;
				break;
			case jmp:
				pc = *(int*)&i.val;
				break;
			case jpc:
				//printf("current stack top is: %d\n", *(int*)s[stack_top].val);
				if (*(int*)s[stack_top].val == 0) {
                    pc = *(int*)&i.val;
					//printf("current pc value is: %d\n", pc);
				}
				stack_top--;
				break;

			default:
				yyerror("Unknown opr occured!\n", 1023);
				break;
		}
	} while(pc != VM_pointer); 
}

int main(int argc, char* argv[]){

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
	if ((fresult = fopen("fresult.txt", "w")) == NULL) {
		printf("Can't open fresult.txt file!\n");
		exit(1);
	}

	redirectInput(fin);		
	init();
	extern int yydebug;
	yydebug = 0;
  	yyparse();
	//printf("yyparse check point1 in main part\n");

	displaytable();	/* 输出符号表 */
	//printf("displaytable check point3 in main part\n");

	listall();  /* 输出所有代码 */
	//printf("listall check point4 in main part\n");

	interpret();	/* 调用解释执行程序 */     
	//printf("interpret check point5 in main part\n");

	fclose(fin);
	fclose(fcode);
	fclose(ftable);
 	fclose(foutput);
	fclose(fresult);
	return 0;
}