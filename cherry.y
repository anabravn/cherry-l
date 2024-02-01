
%{
#include <stdio.h>
#include "cherry.l.h"

%}

%union {
    long int4;              /* Constant integer value */
    float fp;               /* Constant floating point value */
    char *str;              /* Ptr to constant string (strings are malloc'd) */
};

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

%type <str> IDENTIFIER
%type <str> STRING

%%

seq: seq LINEBREAK seq 
   | seq LINEBREAK
   | expr
   |

expr: a_expr { printf("a_expr\n"); }
    | stmt 
    | cmp_expr { printf("cmp_expr\n"); }

stmt: if_stmt   { printf("if_stmt\n"); }
    | for_stmt  { printf("for_stmt\n"); }
    | attr_stmt 
    | decl_stmt { printf("decl_stmt\n"); }
    | fdecl    
    | classdecl { printf("classdecl\n"); }

classdecl: KEYWORD_CLASS IDENTIFIER LINEBREAK
           class_attr
           class_methods
           KEYWORD_END

class_attr: class_attr LINEBREAK class_attr
          | class_attr LINEBREAK
          | decl_stmt { printf("decl_stmt\n"); }
          |

class_methods: class_methods LINEBREAK class_methods
             | class_methods LINEBREAK
             | fdecl
             |


fdecl: KEYWORD_DEF IDENTIFIER LEFT_PAREN params RIGHT_PAREN
       LINEBREAK seq KEYWORD_END { printf("fdecl\n"); } 

params: decl_stmt COLON params
      | decl_stmt
      |


attr_stmt: variable OP_ATTR a_expr { printf("attr_stmt\n"); }


decl_stmt: TYPE IDENTIFIER OP_ATTR a_expr
         | TYPE IDENTIFIER


for_stmt: KEYWORD_FOR cmp_expr LINEBREAK
          seq KEYWORD_END 


if_stmt: KEYWORD_IF cmp_expr LINEBREAK 
         seq if_stmt_end KEYWORD_END 

if_stmt_end: KEYWORD_ELSE seq 
           |


cmp_expr: a_expr cmp_op a_expr 

cmp_op: OP_EQUAL | OP_NEQUAL 
      | OP_GREATER | OP_LESS
      | OP_GREATEREQ | OP_LESSEQ


a_expr: a_expr OP_ADD term 
    | a_expr OP_SUB term 
    | term

term: term OP_MUL factor 
    | term OP_DIV factor 
    | factor

factor: LEFT_PAREN a_expr RIGHT_PAREN 
      | fcall
      | variable
      | STRING { printf("str: %s\n", $1); }
      | INTEGER 


fcall: variable LEFT_PAREN args RIGHT_PAREN { printf("fcall\n"); }

variable: IDENTIFIER PERIOD variable 
        | IDENTIFIER { printf("var: %s\n", $1); }

args: a_expr COLON args
    | a_expr
    |

%%

int yyerror(char const *s)
{
    printf("\nERROR: %s\n", s);
}

int main(void) 
{
 return yyparse();
}  
