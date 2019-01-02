x0: x0.tab.c lex.yy.c
	gcc x0.tab.c lex.yy.c -o $@

lex.yy.c: X0lex.l
	flex X0lex.l

x0.tab.c: X0Compiler.final.v2.y
	bison -d -v --debug X0Compiler.final.v2.y

clean:
	del x0.tab.c
	del x0.tab.h
	del lex.yy.c