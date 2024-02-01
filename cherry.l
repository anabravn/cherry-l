%option noyywrap

%{
#include <math.h>
#include "cherry.y.h"
%}

DIGIT   [0-9]
ID      [A-Za-z][A-Za-z0-9]*
STR  \"([^\\\"]|\\.)*\" 
CHAR    \'\\?.\'

%%

[ \t]+|\n[\n]+ /* whitespace */

    /* punctuation */
"("  return LEFT_PAREN;
")"  return RIGHT_PAREN;
"["  return LEFT_BRACKET;
"]"  return RIGHT_BRACKET;
","  return COLON;
"."  return PERIOD;
\n   return LINEBREAK;

    /* operators */
"==" return OP_EQUAL;
"!=" return OP_NEQUAL;
">=" return OP_GREATEREQ;
"<=" return OP_LESSEQ;
">" return OP_GREATER;
"<" return OP_LESS;

"+" return OP_ADD;
"-" return OP_SUB;
"/" return OP_DIV;
"*" return OP_MUL;

and return OP_AND;
or return OP_OR;

"=" return OP_ATTR;


    /* data literals */
{DIGIT}*"."{DIGIT}+    { sscanf(yytext, " %f", &yylval); return FLOAT; }
{DIGIT}+               { sscanf(yytext, " %d", &yylval); return INTEGER; }
{STR}                  return STRING;
{CHAR}                 return CHARACTER;

    /* keywords */
end     return KEYWORD_END;
if      return KEYWORD_IF;
else    return KEYWORD_ELSE;
for     return KEYWORD_FOR;
def     return KEYWORD_DEF;
class   return KEYWORD_CLASS;

int|float|str|char return TYPE; /* primitive data types */

{ID} return IDENTIFIER; /* identifiers */

.|\n return INVALID_TOKEN; /* error */

