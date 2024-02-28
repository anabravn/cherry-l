
%{
#include <stdio.h>
#include "cherry.l.h"
#include "ast.h"

int yyerror(char const *s)
{
    printf("\nERROR:%d: %s\n", yylineno, s);
}

int root;

%}

%define parse.error verbose
%locations

%union {
    int int4;              /* Constant integer value */
    float fp;               /* Constant floating point value */
    char *str;              /* Ptr to constant string (strings are malloc'd) */
    char ch;
};

%token TK_IDENTIFIER

%token TK_NIL TK_INTEGER TK_FLOAT TK_STRING TK_CHARACTER
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

%type <int4> classdecl class_attrs class_methods class_attr_decl class_method_decl
%type <int4> program seq expr stmt else_stmt array cmp_op
%type <int4> fdecl decl_stmt type params attr_stmt brackets
%type <int4> variable 
%type <int4> a_expr cond_expr a_term cond_term cond_factor a_factor members cmp_expr identifier
%type <int4> for_stmt if_stmt for_head if_head TK_LESS TK_LESSEQ TK_GREATER TK_GREATEREQ TK_EQUAL TK_NEQUAL fcall args

%type <str> TK_TYPE TK_IDENTIFIER TK_STRING
%type <int4> TK_INTEGER
%type <fp> TK_FLOAT
%type <ch> TK_CHARACTER

%%

program: seq { $$ = root = mknode($1, -1, -1, "program", (NodeValue) 0); }

seq: expr { $$ = mknode($1, -1, -1, "seq", (NodeValue) 0); }
   | seq TK_LINEBREAK expr { $$ = mknode($1, $3, -1, "seq", (NodeValue) 0); }

expr: a_expr { $$ = mknode($1, -1, -1, "expr", (NodeValue) 0); }
    | stmt { $$ = mknode($1, -1, -1, "expr", (NodeValue) 0); } 
    | cond_expr { $$ = mknode($1, -1, -1, "expr", (NodeValue) 0); }
    | expr error { $$ = -1; }
    | %empty { $$ = -1; }

stmt: if_stmt   { $$ = mknode($1, -1, -1, "stmt", (NodeValue) 0); }
    | for_stmt  { $$ = mknode($1, -1, -1, "stmt", (NodeValue) 0); }
    | attr_stmt { $$ = mknode($1, -1, -1, "stmt", (NodeValue) 0); } 
    | decl_stmt { $$ = mknode($1, -1, -1, "stmt", (NodeValue) 0); }
    | fdecl     { $$ = mknode($1, -1, -1, "stmt", (NodeValue) 0); }
    | classdecl { $$ = mknode($1, -1, -1, "stmt", (NodeValue) 0); }

classdecl: KEYWORD_CLASS identifier TK_LINEBREAK
           class_attrs
           class_methods
           KEYWORD_END
           { $$ = mknode($2, $4, $5, "classdecl", (NodeValue) 0); }

class_attrs: class_attr_decl { $$ = mknode($1, -1, -1, "class_attrs", (NodeValue) 0); }
          | class_attrs TK_LINEBREAK class_attr_decl { $$ = mknode($1, $3, -1, "class_attrs", (NodeValue) 0); }

class_attr_decl: decl_stmt { $$ = mknode($1, -1, -1, "class_attr_decl", (NodeValue) 0); }
               | %empty { $$ = -1; }

class_methods: class_method_decl { $$ = mknode($1, -1, -1, "class_methods", (NodeValue) 0); }
             | class_methods TK_LINEBREAK class_method_decl { $$ = mknode($1, $3, -1, "class_methods", (NodeValue) 0); }


class_method_decl: fdecl { $$ = mknode($1, -1, -1, "class_method_decl", (NodeValue) 0); }
                 | %empty { $$ = -1; }

fdecl: KEYWORD_DEF identifier TK_LEFT_PAREN params TK_RIGHT_PAREN 
       TK_LINEBREAK seq KEYWORD_END 
       { $$ = mknode($2, $4, $7, "fdecl", (NodeValue) 0); } 

params: decl_stmt TK_COLON params { $$ = mknode($1, $3, -1, "params", (NodeValue) 0); }
      | decl_stmt { $$ = mknode($1, -1, -1, "params", (NodeValue) 0); }
      | %empty { $$ = -1; }


attr_stmt: variable TK_ATTR a_expr { $$ = mknode($1, $3, -1, "attr_stmt", (NodeValue) 0); }

type: TK_TYPE brackets { $$ = mknode($2, -1, -1, "type", (NodeValue) $1); }

brackets: TK_LEFT_BRACKET TK_RIGHT_BRACKET brackets { $$ = mknode($3, -1, -1, "[]", (NodeValue) 0); }
        | %empty { $$ = -1; }

decl_stmt: type identifier TK_ATTR a_expr { $$ = mknode($1, $2, $4, "decl_stmt", (NodeValue) 0); }
         | type identifier { $$ = mknode($1, $2, -1, "decl_stmt", (NodeValue) 0); }


for_stmt: KEYWORD_FOR for_head TK_LINEBREAK
          seq KEYWORD_END 
          { $$ = mknode($2, $4, -1, "for_stmt", (NodeValue) 0); }

for_head: cond_expr { $$ = mknode($1, -1, -1, "for_head", (NodeValue) 0); }
        | a_expr { $$ = mknode($1, -1, -1, "for_head", (NodeValue) 0); }
        | attr_stmt TK_COLON a_expr { $$ = mknode($1, $3, -1, "for_head", (NodeValue) 0); }
        | attr_stmt TK_COLON a_expr TK_COLON a_expr { $$ = mknode($1, $3, $5, "for_head", (NodeValue) 0); }
       
if_stmt: if_head 
         seq else_stmt KEYWORD_END { $$ = mknode($1, $2, $3, "if_stmt", (NodeValue) 0); }

if_head: KEYWORD_IF cond_expr TK_LINEBREAK { $$ = mknode($2, -1, -1, "if_head", (NodeValue) 0); }
       | KEYWORD_IF a_expr TK_LINEBREAK { $$ = mknode($2, -1, -1, "if_head", (NodeValue) 0); }
        

else_stmt: KEYWORD_ELSE seq  {$$ = mknode($2, -1, -1, "else_stmt", (NodeValue) 0); }
           | %empty { $$ = 0; }

cond_expr: cond_expr TK_OR cond_term { $$ = mknode($1, $3, -1, "cond_expr_or", (NodeValue) 0); }
         | a_expr TK_OR a_expr { $$ = mknode($1, $3, -1, "cond_expr_or", (NodeValue) 0); }
         | cond_term { $$ = mknode($1, -1, -1, "cond_expr", (NodeValue) 0); }

cond_term: cond_term TK_AND cond_factor { $$ = mknode($1, $3, -1, "cond_expr_and", (NodeValue) 0); }
         | a_expr TK_AND a_expr { $$ = mknode($1, $3, -1, "cond_expr_and", (NodeValue) 0); }
         | cond_factor { $$ = mknode($1, -1, -1, "cond_term", (NodeValue) 0); }

cond_factor: TK_LEFT_PAREN cond_expr TK_RIGHT_PAREN { $$ = mknode($2, -1, -1, "cond_factor", (NodeValue) 0); }
           | cmp_expr { $$ = mknode($1, -1, -1, "cond_factor", (NodeValue) 0); }

cmp_expr: a_expr cmp_op a_expr { $$ = mknode($1, $2, $3, "cmp_expr", (NodeValue) 0); } 

cmp_op: TK_EQUAL { $$ = mknode(-1, -1, -1, "==", (NodeValue) 0); }
      | TK_NEQUAL { $$ = mknode(-1, -1, -1, "!=", (NodeValue) 0); }
      | TK_GREATER { $$ = mknode(-1, -1, -1, ">", (NodeValue) 0); }
      | TK_LESS { $$ = mknode(-1, -1, -1, "<", (NodeValue) 0); }
      | TK_GREATEREQ { $$ = mknode(-1, -1, -1, ">=", (NodeValue) 0); }
      | TK_LESSEQ { $$ = mknode(-1, -1, -1, "<=", (NodeValue) 0); }


a_expr: a_expr TK_ADD a_term { $$ = mknode($1, $3, -1, "expr_add", (NodeValue) 0); }
    | a_expr TK_SUB a_term { $$ = mknode($1, $3, -1, "expr_sub", (NodeValue) 0); }
    | a_term { $$ = mknode($1, -1, -1, "a_expr", (NodeValue) 0); }

a_term: a_term TK_MUL a_factor { $$ = mknode($1, $3, -1, "expr_mul", (NodeValue) 0); } 
    | a_term TK_DIV a_factor { $$ = mknode($1, $3, -1, "expr_mul", (NodeValue) 0); }
    | a_factor { $$ = mknode($1, -1, -1, "a_term", (NodeValue) 0); }

a_factor: TK_LEFT_PAREN a_expr TK_RIGHT_PAREN { $$ = mknode($2, -1, -1, "a_factor", (NodeValue) 0); }
      | fcall { $$ = mknode($1, -1, -1, "a_factor", (NodeValue) 0); }
      | variable { $$ = mknode($1, -1, -1, "a_factor", (NodeValue) 0); }
      | array { $$ = mknode($1, -1, -1, "a_factor", (NodeValue) 0); }
      | TK_STRING { $$ = mknode(-1, -1, -1, "string", (NodeValue) $1); }
      | TK_CHARACTER { $$ = mknode(-1, -1, -1, "char", (NodeValue) $1); }
      | TK_INTEGER { $$ = mknode(-1, -1, -1, "int", (NodeValue) $1); }
      | TK_FLOAT { $$ = mknode(-1, -1, -1, "float", (NodeValue) $1); }
      | TK_NIL { $$ = mknode(-1, -1, -1, "nil", (NodeValue) 0); }


variable: identifier TK_PERIOD variable { $$ = mknode($1, $3, -1, "variable", (NodeValue) 0); }
        | identifier { $$ = mknode($1, -1, -1, "variable", (NodeValue) 0); }


fcall: variable TK_LEFT_PAREN args TK_RIGHT_PAREN { $$ = mknode($1, $3, -1, "fcall", (NodeValue) 0); }

args: a_expr TK_COLON args { $$ = mknode($1, $3, -1, "args", (NodeValue) 0); }
    | a_expr { $$ = mknode($1, -1, -1, "args", (NodeValue) 0); }
    | %empty { $$ = -1; }


array: TK_LEFT_BRACKET members TK_RIGHT_BRACKET { $$ = mknode($2, -1, -1, "array", (NodeValue) 0); }

members: a_expr TK_COLON members { $$ = mknode($1, $3, -1, "members", (NodeValue) 0); }
       | a_expr { $$ = mknode($1, -1, -1, "members", (NodeValue) 0); }
       | %empty { $$ = -1; }

identifier: TK_IDENTIFIER { $$ = mknode(-1, -1, -1, "identifier", (NodeValue) $1); }

%%
int main(void) 
{
    yyparse();

    putchar('\n');
    print_nodes();
    putchar('\n');
    print_tree(root, 0);
    free_tree();
}  
