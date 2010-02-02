%{
#include "parser.h"
#include "yacclib.h"
#include "rml.h"
#include "Assignment.h"

typedef void *rml_t;
/* extern rml_t yylval; */

int lex_icon();
int lex_ident();

%}

whitespace   [ \t\n]+
letter       [a-zA-Z_]
ident           {letter}({letter}|{digit})*
digit        [0-9]
digits       {digit}+
%%

{whitespace} ;
{ident}         return lex_ident(); /* T_IDENT */
{digits}     return lex_icon();  /* T_INTCONST */
":="         return T_ASSIGN;
"+"          return T_ADD;
"-"          return T_SUB;
"*"          return T_MUL;
"/"          return T_DIV;
"("          return T_LPAREN;
")"          return T_RPAREN;
";"          return T_SEMIC;

%%

int lex_icon()
{
  yylval = (rml_t) Assignment__INT(mk_icon(atoi(yytext)));  
  return T_INTCONST;        
}

int lex_ident()
{
        yylval = (rml_t) mk_scon(yytext);
        return T_IDENT;
}
