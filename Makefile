cherry: lexer cherry.y
	bison -Wcounterexamples -Wall --defines=cherry.y.h -o cherry.y.c cherry.y
	gcc *.c -o cherry
	
lexer: cherry.l
	lex --header-file=cherry.l.h -o cherry.l.c cherry.l
