
%{
#include <stdio.h>
#include "cherry.l.h"
#include "ast.h"

int yyerror(char const *s)
{
    printf("\nERROR: %s\n", s);
}

int root;

%}

%union {
    long int4;              /* Constant integer value */
    float fp;               /* Constant floating point value */
    char *str;              /* Ptr to constant string (strings are malloc'd) */
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
%type <int4> TK_IDENTIFIER TK_TYPE variable TK_STRING TK_CHARACTER TK_FLOAT TK_INTEGER
%type <int4> a_expr cond_expr a_term cond_term cond_factor factor members cmp_expr
%type <int4> for_stmt if_stmt for_head if_head TK_LESS TK_LESSEQ TK_GREATER TK_GREATEREQ TK_EQUAL TK_NEQUAL fcall args

%%

program: seq { $$ = root = mknode($1, -1, -1, "program"); }

seq: expr { $$ = mknode($1, -1, -1, "seq"); }
   | seq TK_LINEBREAK expr { $$ = mknode($1, $3, -1, "seq"); }

expr: a_expr { $$ = mknode($1, -1, -1, "expr"); }
    | stmt { $$ = mknode($1, -1, -1, "expr"); } 
    | cond_expr { $$ = mknode($1, -1, -1, "expr"); }
    | %empty { $$ = -1; }

stmt: if_stmt   { $$ = mknode($1, -1, -1, "stmt"); }
    | for_stmt  { $$ = mknode($1, -1, -1, "stmt"); }
    | attr_stmt { $$ = mknode($1, -1, -1, "stmt"); } 
    | decl_stmt { $$ = mknode($1, -1, -1, "stmt"); }
    | fdecl     { $$ = mknode($1, -1, -1, "stmt"); }
    | classdecl { $$ = mknode($1, -1, -1, "stmt"); }

classdecl: KEYWORD_CLASS TK_IDENTIFIER TK_LINEBREAK
           class_attrs
           class_methods
           KEYWORD_END
           { $$ = mknode($4, $5, -1, "classdecl"); }

class_attrs: class_attr_decl { $$ = mknode($1, -1, -1, "class_attrs"); }
          | class_attrs TK_LINEBREAK class_attr_decl { $$ = mknode($1, $3, -1, "class_attrs"); }

class_attr_decl: decl_stmt { $$ = mknode($1, -1, -1, "class_attr_decl"); }
               | %empty { $$ = -1; }

class_methods: class_method_decl { $$ = mknode($1, -1, -1, "class_methods"); }
             | class_methods TK_LINEBREAK class_method_decl { $$ = mknode($1, $3, -1, "class_methods"); }


class_method_decl: fdecl { $$ = mknode($1, -1, -1, "class_method_decl"); }
                 | %empty { $$ = -1; }

fdecl: KEYWORD_DEF TK_IDENTIFIER TK_LEFT_PAREN params TK_RIGHT_PAREN 
       TK_LINEBREAK seq KEYWORD_END 
       { $$ = mknode($4, $7, -1, "fdecl"); } 

params: decl_stmt TK_COLON params { $$ = mknode($1, $3, -1, "params"); }
      | decl_stmt { $$ = mknode($1, -1, -1, "params"); }
      | %empty { $$ = -1; }


attr_stmt: variable TK_ATTR a_expr { $$ = mknode($1, $3, -1, "attr_stmt"); }


type: TK_TYPE brackets { $$ = mknode($2, -1, -1, "type"); }
    | TK_IDENTIFIER brackets { $$ = mknode($2, -1, -1, "type"); }


brackets: TK_LEFT_BRACKET TK_RIGHT_BRACKET brackets { $$ = mknode($3, -1, -1, "[]"); }
        | %empty { $$ = -1; }


decl_stmt: type TK_IDENTIFIER TK_ATTR a_expr { $$ = mknode($1, $4, -1, "decl_stmt"); }
         | type TK_IDENTIFIER { $$ = mknode($1, -1, -1, "decl_stmt"); }


for_stmt: KEYWORD_FOR for_head TK_LINEBREAK
          seq KEYWORD_END 
          { $$ = mknode($2, $4, -1, "for_stmt"); }

for_head: cond_expr { $$ = mknode($1, -1, -1, "for_head"); }
        | a_expr { $$ = mknode($1, -1, -1, "for_head"); }
        | attr_stmt TK_COLON a_expr { $$ = mknode($1, $3, -1, "for_head"); }
        | attr_stmt TK_COLON a_expr TK_COLON a_expr { $$ = mknode($1, $3, $5, "for_head"); }
       
if_stmt: if_head 
         seq else_stmt KEYWORD_END { $$ = mknode($1, $2, $3, "if_stmt"); }

if_head: KEYWORD_IF cond_expr TK_LINEBREAK { $$ = mknode($2, -1, -1, "if_head"); }
       | KEYWORD_IF a_expr TK_LINEBREAK { $$ = mknode($2, -1, -1, "if_head"); }
        

else_stmt: KEYWORD_ELSE seq else_stmt {$$ = mknode($2, -1, -1, "else_stmt"); }
           | %empty { $$ = 0; }

cond_expr: cond_expr TK_OR cond_term { $$ = mknode($1, $3, -1, "cond_expr_or"); }
         | a_expr TK_OR a_expr { $$ = mknode($1, $3, -1, "cond_expr_or"); }
         | cond_term { $$ = mknode($1, -1, -1, "cond_expr"); }

cond_term: cond_term TK_AND cond_factor { $$ = mknode($1, $3, -1, "cond_expr_and"); }
         | a_expr TK_AND a_expr { $$ = mknode($1, $3, -1, "cond_expr_and"); }
         | cond_factor { $$ = mknode($1, -1, -1, "cond_term"); }

cond_factor: TK_LEFT_PAREN cond_expr TK_RIGHT_PAREN { $$ = mknode($2, -1, -1, "cond_factor"); }
           | cmp_expr { $$ = mknode($1, -1, -1, "cond_factor"); }

cmp_expr: a_expr cmp_op a_expr { $$ = mknode($1, $2, $3, "cmp_expr"); } 

cmp_op: TK_EQUAL { $$ = mknode(-1, -1, -1, "=="); }
      | TK_NEQUAL { $$ = mknode(-1, -1, -1, "!="); }
      | TK_GREATER { $$ = mknode(-1, -1, -1, ">"); }
      | TK_LESS { $$ = mknode(-1, -1, -1, "<"); }
      | TK_GREATEREQ { $$ = mknode(-1, -1, -1, ">="); }
      | TK_LESSEQ { $$ = mknode(-1, -1, -1, "<="); }


a_expr: a_expr TK_ADD a_term { $$ = mknode($1, $3, -1, "expr_add"); }
    | a_expr TK_SUB a_term { $$ = mknode($1, $3, -1, "expr_sub"); }
    | a_term { $$ = mknode($1, -1, -1, "a_expr"); }

a_term: a_term TK_MUL factor { $$ = mknode($1, $3, -1, "expr_mul"); } 
    | a_term TK_DIV factor { $$ = mknode($1, $3, -1, "expr_mul"); }
    | factor { $$ = mknode($1, -1, -1, "a_term"); }

factor: TK_LEFT_PAREN a_expr TK_RIGHT_PAREN { $$ = mknode($2, -1, -1, "factor"); }
      | fcall { $$ = mknode($1, -1, -1, "factor"); }
      | variable { $$ = mknode($1, -1, -1, "factor"); }
      | array { $$ = mknode($1, -1, -1, "factor"); }
      | TK_STRING { $$ = mknode(-1, -1, -1, "string"); }
      | TK_CHARACTER { $$ = mknode(-1, -1, -1, "char"); }
      | TK_INTEGER { $$ = mknode(-1, -1, -1, "integer"); }
      | TK_FLOAT { $$ = mknode(-1, -1, -1, "float"); }
      | TK_NIL { $$ = mknode(-1, -1, -1, "nil"); }


variable: TK_IDENTIFIER TK_PERIOD variable { $$ = mknode($3, -1, -1, "variable"); }
        | TK_IDENTIFIER { $$ = mknode(-1, -1, -1, "variable"); }


fcall: variable TK_LEFT_PAREN args TK_RIGHT_PAREN { $$ = mknode($1, $3, -1, "fcall"); }

args: a_expr TK_COLON args { $$ = mknode($1, $3, -1, "args"); }
    | a_expr { $$ = mknode($1, -1, -1, "args"); }
    | %empty { $$ = -1; }


array: TK_LEFT_BRACKET members TK_RIGHT_BRACKET { $$ = mknode($2, -1, -1, "array"); }

members: a_expr TK_COLON members { $$ = mknode($1, $3, -1, "members"); }
       | a_expr { $$ = mknode($1, -1, -1, "members"); }
       | %empty { $$ = -1; }

%%
int main(void) 
{
    if(yyparse()) return 1;

    putchar('\n');
    putchar('\n');
    print_tree(root, 0);
    free_tree();
}  
