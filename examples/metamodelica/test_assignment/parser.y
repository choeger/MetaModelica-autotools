%{
#include <stdio.h>
#include "yacclib.h"
#include "rml.h"
#include "Assignment.h"

typedef void *rml_t;
#define YYSTYPE rml_t
extern rml_t absyntree;

%}

%token T_SEMIC
%token T_ASSIGN
%token T_IDENT
%token T_INTCONST
%token T_LPAREN T_RPAREN
%token T_ADD
%token T_SUB
%token T_MUL
%token T_DIV
%token T_GARBAGE

%token T_ERR

%%

/* Yacc BNF grammar of the expression language Assignments */

program         :  assignments T_SEMIC expression
                        { absyntree = Assignment__PROGRAM($1, $3);}

assignments     :  assignments  assignment
                        { $$ = mk_cons($2, $1);}
                |
                        { $$ = mk_nil();}

assignment      :  T_IDENT  T_ASSIGN  expression
                        { $$ = Assignment__ASSIGN($1, $3);}

expression      :  term
                        { $$ = $1;}
                |  expression  weak_operator  term
                        { $$ = Assignment__BINARY($1, $2, $3);}

term            :  u_element
                        { $$ = $1;}
                |  term  strong_operator  u_element
                        { $$ = Assignment__BINARY($1, $2, $3);}

u_element       :  element
                        { $$ = $1;}
                |  unary_operator  element
                        { $$ = Assignment__UNARY($1, $2);}

element         :  T_INTCONST
                        { $$ = $1;}
                |  T_IDENT
                        { $$ = Assignment__IDENT($1);}
                |  T_LPAREN  expression  T_RPAREN
                        { $$ = $2;}
                |  T_LPAREN  assignment  T_RPAREN
                        { $$ = $2;}

weak_operator   :  T_ADD
                        { $$ = Assignment__ADD;}
                |  T_SUB
                        { $$ = Assignment__SUB;}

strong_operator :  T_MUL
                        { $$ = Assignment__MUL;}
                |  T_DIV
                        { $$ = Assignment__DIV;}

unary_operator  :  T_SUB
                        { $$ = Assignment__NEG;}


