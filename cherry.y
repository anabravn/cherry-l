
%{
#include <stdio.h>
#include "cherry.l.h"
%}

%token IDENTIFIER
%token INVALID_TOKEN

%token INTEGER FLOAT STRING CHARACTER
%token TYPE

%token KEYWORD_IF KEYWORD_FOR KEYWORD_ELSE
%token KEYWORD_DEF KEYWORD_CLASS KEYWORD_END

%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LINEBREAK COLON PERIOD

%token OP_EQUAL OP_NEQUAL
%token OP_GREATEREQ OP_LESSEQ
%token OP_GREATER OP_LESS

%token OP_ADD OP_SUB OP_DIV OP_MUL
%token OP_AND OP_OR 
%token OP_ATTR


%%

program: expr LINEBREAK program
       | expr

expr: expr OP_ADD term { printf("expr\n"); }
    | expr OP_SUB term { printf("expr\n"); }
    | term

term: term OP_MUL factor { printf("term\n"); }
    | term OP_DIV factor { printf("term\n"); }
    | factor

factor: LEFT_PAREN expr RIGHT_PAREN { printf("factor\n"); }
      | INTEGER { printf("factor\n"); }


%%

int yyerror(char const *s)
{
    printf("\nERROR: %s\n", s);
}

int main(void) 
{
 return yyparse();
}  
