
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

%token TK_IDENTIFIER

%token TK_INTEGER TK_FLOAT TK_STRING TK_CHARACTER TK_NIL
%token TK_TYPE

%token KEYWORD_IF KEYWORD_FOR KEYWORD_ELSE
%token KEYWORD_DEF KEYWORD_CLASS KEYWORD_END

%token TK_LEFT_PAREN TK_RIGHT_PAREN
%token TK_LEFT_BRACKET TK_RIGHT_BRACKET
%token TK_LINEBREAK TK_COLON TK_PERIOD

%token TK_EQUAL TK_NEQUAL
%token TK_GREATEREQ TK_LESSEQ
%token TK_GREATER TK_LESS

%token TK_ADD TK_SUB TK_DIV TK_MUL
%token TK_AND TK_OR 
%token TK_ATTR

%type <str> TK_IDENTIFIER
%type <str> TK_STRING
%type <float> TK_FLOAT
%type <int> TK_INTEGER

%%

seq: expr
   | seq TK_LINEBREAK expr

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

classdecl: KEYWORD_CLASS TK_IDENTIFIER TK_LINEBREAK
           class_attrs
           class_methods
           KEYWORD_END

class_attrs: class_attr_decl
          | class_attrs TK_LINEBREAK class_attr_decl

class_attr_decl: decl_stmt
               | %empty

class_methods: class_method_decl
             | class_methods TK_LINEBREAK class_method_decl


class_method_decl: fdecl
                 | %empty

fdecl: KEYWORD_DEF TK_IDENTIFIER TK_LEFT_PAREN params TK_RIGHT_PAREN
       TK_LINEBREAK seq KEYWORD_END { printf("fdecl\n"); } 

params: decl_stmt TK_COLON params
      | decl_stmt
      | %empty


attr_stmt: variable TK_ATTR a_expr { printf("attr_stmt\n"); }


type: TK_TYPE brackets
    | TK_IDENTIFIER brackets


brackets: TK_LEFT_BRACKET TK_RIGHT_BRACKET brackets
        | %empty


decl_stmt: type TK_IDENTIFIER TK_ATTR a_expr
         | type TK_IDENTIFIER


for_stmt: KEYWORD_FOR for_head TK_LINEBREAK
          seq KEYWORD_END 

for_head: cond_expr | a_expr
        | attr_stmt TK_COLON a_expr
        | attr_stmt TK_COLON a_expr TK_COLON a_expr
       
if_stmt: if_head
         seq if_stmt_end KEYWORD_END 

if_head: KEYWORD_IF cond_expr TK_LINEBREAK 
       | KEYWORD_IF a_expr TK_LINEBREAK
        

if_stmt_end: KEYWORD_ELSE seq 
           | %empty



cond_expr: cond_expr TK_OR cond_term
         | a_expr TK_OR a_expr
         | cond_term

cond_term: cond_term TK_AND cond_factor
         | a_expr TK_AND a_expr
         | cond_factor

cond_factor: TK_LEFT_PAREN cond_expr TK_RIGHT_PAREN
           | cmp_expr

cmp_expr: a_expr cmp_op a_expr 

cmp_op: TK_EQUAL | TK_NEQUAL 
      | TK_GREATER | TK_LESS
      | TK_GREATEREQ | TK_LESSEQ


a_expr: a_expr TK_ADD a_term 
    | a_expr TK_SUB a_term 
    | a_term

a_term: a_term TK_MUL factor 
    | a_term TK_DIV factor 
    | factor

factor: TK_LEFT_PAREN a_expr TK_RIGHT_PAREN 
      | fcall
      | variable
      | array
      | TK_STRING { printf("str: %s\n", $1); }
      | TK_CHARACTER
      | TK_INTEGER 
      | TK_NIL


variable: TK_IDENTIFIER TK_PERIOD variable 
        | TK_IDENTIFIER { printf("var: %s\n", $1); }


fcall: variable TK_LEFT_PAREN args TK_RIGHT_PAREN { printf("fcall\n"); }

args: a_expr TK_COLON args
    | a_expr
    | %empty


array: TK_LEFT_BRACKET members TK_RIGHT_BRACKET { printf("array\n"); }

members: a_expr TK_COLON members
       | a_expr
       | %empty

%%
int main(void) 
{
 return yyparse();
}  
