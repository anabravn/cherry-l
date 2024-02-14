
%{
#include <stdio.h>
#include "cherry.l.h"

int yyerror(char const *s)
{
    printf("\nERROR: %s\n", s);
}


%}

%union {
    long int4;              /* Constant integer value */
    float fp;               /* Constant floating point value */
    char *str;              /* Ptr to constant string (strings are malloc'd) */
};

%token IDENTIFIER

%token INTEGER FLOAT STRING CHARACTER NIL
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
%type <float> FLOAT
%type <int> INTEGER

%%

seq: expr
   | seq LINEBREAK expr

expr: a_expr { printf("a_expr\n"); }
    | stmt 
    | cond_expr { printf("cond_expr\n"); }
    | %empty

stmt: if_stmt   { printf("if_stmt\n"); }
    | for_stmt  { printf("for_stmt\n"); }
    | attr_stmt 
    | decl_stmt { printf("decl_stmt\n"); }
    | fdecl    
    | classdecl { printf("classdecl\n"); }

classdecl: KEYWORD_CLASS IDENTIFIER LINEBREAK
           class_attrs
           class_methods
           KEYWORD_END

class_attrs: class_attr_decl
          | class_attrs LINEBREAK class_attr_decl

class_attr_decl: decl_stmt
               | %empty

class_methods: class_method_decl
             | class_methods LINEBREAK class_method_decl


class_method_decl: fdecl
                 | %empty

fdecl: KEYWORD_DEF IDENTIFIER LEFT_PAREN params RIGHT_PAREN
       LINEBREAK seq KEYWORD_END { printf("fdecl\n"); } 

params: decl_stmt COLON params
      | decl_stmt
      | %empty


attr_stmt: variable OP_ATTR a_expr { printf("attr_stmt\n"); }


type: TYPE brackets
    | IDENTIFIER brackets


brackets: LEFT_BRACKET RIGHT_BRACKET brackets
        | %empty


decl_stmt: type IDENTIFIER OP_ATTR a_expr
         | type IDENTIFIER


for_stmt: KEYWORD_FOR for_head LINEBREAK
          seq KEYWORD_END 

for_head: cond_expr | a_expr
        | attr_stmt COLON a_expr
        | attr_stmt COLON a_expr COLON a_expr
       
if_stmt: if_head
         seq if_stmt_end KEYWORD_END 

if_head: KEYWORD_IF cond_expr LINEBREAK 
       | KEYWORD_IF a_expr LINEBREAK
        

if_stmt_end: KEYWORD_ELSE seq 
           | %empty



cond_expr: cond_expr OP_OR cond_term
         | a_expr OP_OR a_expr
         | cond_term

cond_term: cond_term OP_AND cond_factor
         | a_expr OP_AND a_expr
         | cond_factor

cond_factor: LEFT_PAREN cond_expr RIGHT_PAREN
           | cmp_expr

cmp_expr: a_expr cmp_op a_expr 

cmp_op: OP_EQUAL | OP_NEQUAL 
      | OP_GREATER | OP_LESS
      | OP_GREATEREQ | OP_LESSEQ


a_expr: a_expr OP_ADD a_term 
    | a_expr OP_SUB a_term 
    | a_term

a_term: a_term OP_MUL factor 
    | a_term OP_DIV factor 
    | factor

factor: LEFT_PAREN a_expr RIGHT_PAREN 
      | fcall
      | variable
      | array
      | STRING { printf("str: %s\n", $1); }
      | CHARACTER
      | INTEGER 
      | NIL


variable: IDENTIFIER PERIOD variable 
        | IDENTIFIER { printf("var: %s\n", $1); }


fcall: variable LEFT_PAREN args RIGHT_PAREN { printf("fcall\n"); }

args: a_expr COLON args
    | a_expr
    | %empty


array: LEFT_BRACKET members RIGHT_BRACKET { printf("array\n"); }

members: a_expr COLON members
       | a_expr
       | %empty

%%
int main(void) 
{
 return yyparse();
}  
