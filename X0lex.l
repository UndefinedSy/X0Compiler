%{
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "malloc.h"
#include "x0yacc.tab.h"

void comment();
int line=1;
void yyerror(char *);
int yywrap(void);
%}

%%
"/*"	{comment();}
[ \t]	{;}
[\n]	{line++;}

"bool"  {return BOOLSYM;}
"break"	{return BREAKSYM;}
"case"	{return CASESYM;}
"char"  {return CHARSYM;}
"const"	{return CONSTSYM;}
"continue"	{return CONTINUESYM;}
"do"	{return DOSYM;}
"default"   {return DEFAULTSYM;} 
"else"	{return ELSESYM;}
"for"	{return FORSYM;}
"if"	{return IFSYM;}
"int"	{return INTSYM;}
"main"  {return MAINSYM;}
"read"	{return READSYM;}
"repeat"    {return REPEATSYM;}
"switch"	{return SWITCHSYM;}
"string"    {return STRINGSYM;}
"then"	{return THENSYM;}
"unsigned"  {return UNSIGNEDSYM;}
"until" {return UNTILSYM;}
"while"	{return WHILESYM;}
"write"	{return WRITESYM;}
"return"	{return RETURNSYM;}

"true"  {
    yylval.boolean = 1;
    return BOOL;
}
"false" {
    yylval.boolean = 0;
    return BOOL;
}

[A-Za-z_]([A-Za-z]|[0-9]|_)*	{
    yylval.ident=(char *)malloc(strlen(yytext)+1);
    strcpy(yylval.ident,yytext);
    return IDENT;
}
                            
[+|-]?[0-9]+	{
    yylval.number=atoi(yytext);
    return NUMBER;
}

["].*["]    {
    yylval.string_chars = (char*)malloc(strlen(yytext) - 1);
    int i, j = 0;
    for (i = 1; i < strlen(yytext) - 1; ++i) {
        yylval.string_chars[j++] = yytext[i];
    }
    yylval.string_chars[strlen(yytext) - 1] = '\0';
    return STRING;
}

['].['] {
    yylval.single_char = yytext[1];
    return CHAR;
}

":="    {return BECOMES;}
"<"	    {return LSS;}
"<="	{return LEQ;}
">" 	{return GTR;}
">="	{return GEQ;}
"=="    {return EQL;}
"!="    {return NEQ;}
"+"	    {return PLUS;}
"++"    {return INC;}
"-"	    {return MINUS;}
"--"    {return DEC;}
"*"	    {return TIMES;}
"/"	    {return	SLASH;}
"%"     {return MOD;}
"("	    {return	LPAREN;}
")"	    {return	RPAREN;}
"["     {return LBRACK;}
"]"     {return RBRACK;}
"{"     {return LBPAREN;}
"}"     {return RBPAREN;}
"&&"	{return	AND;}
"||"	{return	OR;}
"!" 	{return	NOT;}
"^^"	{return	XOR;}
","	    {return COMMA;}
"."	    {return PERIOD;}
":"     {return COLON;}
";"	    {return	SEMICOLON;}
.	    {yyerror("Unknown character!");}

%%

void redirectInput(FILE *input) {
	yyin=input;
}

int yygetchar(void) {
	int ret;
	ret = getc(yyin);
	printf("%c",ret);
	return ret;
}

int yywrap(void) {
	return 1;
}

void comment() {
	line = 0;
	char c, prev = 0;
	
	while (c = getchar()) {
		if (c == '/' && prev == '*')
			return;
		prev = c;
	}
	printf("unterminated comment");
}