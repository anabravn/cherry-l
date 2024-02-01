%option noyywrap

%{
#include <stdio.h>
#include <math.h>
%}

DIGIT   [0-9]
ID      [A-Za-z][A-Za-z0-9]*
STR  \"([^\\\"]|\\.)*\" 
CHAR    \'\\?.\'

%%

[ \t]+|\n[\n]+  /* whitespace */

    /* punctuation */
"("  { printf("LEFT_PAREN\n"); }
")"  { printf("RIGHT_PAREN\n"); }
"["  { printf("LEFT_BRACKET\n"); }
"]"  { printf("RIGHT_BRACKER\n"); }
","  { printf("COLON\n"); }
"."  { printf("PERIOD\n"); }
\n   { printf("LINEBREAK\n"); }


    /* operators */

"==" { printf("OP_EQUAL\n"); }
"!=" { printf("OP_NEQUAL\n"); }
">=" { printf("OP_GREATEREQ\n"); }
"<=" { printf("OP_LESSEQ\n"); }
">" { printf("OP_GREATER\n"); }
"<" { printf("OP_LESS\n"); }

"+" { printf("OP_ADD\n"); }
"-" { printf("OP_SUB\n"); }
"/" { printf("OP_DIV\n"); } 
"*" { printf("OP_MUL\n"); }

and { printf("OP_AND\n"); }
or { printf("OP_OR\n"); }

"=" { printf("OP_ATTR\n"); }


    /* data literals */
{DIGIT}+                { printf("INTEGER\t%d\n", atoi(yytext)); }
{DIGIT}*"."{DIGIT}+     { printf("FLOAT\t%.2f\n", atof(yytext)); }
{STR}                   { printf("STRING\t%s\n", yytext); }
{CHAR}                  { printf("CHARACTER\t%s\n", yytext); } 

    /* keywords */
end     { printf("KEYWORD_END\n"); }
if      { printf("KEYWORD_IF\n"); }
else    { printf("KEYWORD_ELSE\n"); }
for     { printf("KEYWORD_FOR\n"); }
def     { printf("KEYWORD_DEF\n"); }
class   { printf("KEYWORD_CLASS\n"); }

    /* primitive data types */
int|float|str|char     { printf("TYPE\t%s\n", yytext); }

    /* identifiers */
{ID} { printf("IDENTIFIER\t%s\n", yytext); }

.|\n { printf("INVALID_TOKEN\n%s\n", yytext); }

%%

int main() {
    yylex();
}   
