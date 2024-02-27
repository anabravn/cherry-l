%option noyywrap

%{
#include <math.h>
#include <string.h>
#include "cherry.y.h"
%}

DIGIT   [0-9]
ID      [A-Za-z_][A-Za-z0-9_]*
STR  \"([^\\\"]|\\.)*\" 
CHAR    \'\\?.\'

%%

[ \t]+ /* whitespace */

    /* punctuation */
"("  return TK_LEFT_PAREN;
")"  return TK_RIGHT_PAREN;
"["  return TK_LEFT_BRACKET;
"]"  return TK_RIGHT_BRACKET;
","  return TK_COLON;
"."  return TK_PERIOD;
\n   return TK_LINEBREAK;

    /* operators */
"==" return TK_EQUAL;
"!=" return TK_NEQUAL;
">=" return TK_GREATEREQ;
"<=" return TK_LESSEQ;
">" return TK_GREATER;
"<" return TK_LESS;

"+" return TK_ADD;
"-" return TK_SUB;
"/" return TK_DIV;
"*" return TK_MUL;

and return TK_AND;
or return TK_OR;

"=" return TK_ATTR;


    /* data literals */
-?{DIGIT}*"."{DIGIT}+    { yylval.int4 = atoi(yytext); return TK_FLOAT; }
-?{DIGIT}+               { yylval.int4 = atof(yytext); return TK_INTEGER; }
{STR}                  { yylval.str = strdup(yytext); return TK_STRING; }
{CHAR}                 return TK_CHARACTER;
"nil"                  return TK_NIL;

    /* keywords */
end     return KEYWORD_END;
if      return KEYWORD_IF;
else    return KEYWORD_ELSE;
for     return KEYWORD_FOR;
def     return KEYWORD_DEF;
class   return KEYWORD_CLASS;

int|float|str|char return TK_TYPE; /* primitive data types */

    /* identifiers */
{ID} { yylval.str = strdup(yytext); return TK_IDENTIFIER; }



