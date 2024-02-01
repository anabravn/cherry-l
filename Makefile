cherry: lexer cherry.y
	yacc --defines=cherry.y.h -o cherry.y.c cherry.y
	gcc *.c -o cherry
	rm *.c *.h

lexer: cherry.l
	lex --header-file=cherry.l.h -o cherry.l.c cherry.l
