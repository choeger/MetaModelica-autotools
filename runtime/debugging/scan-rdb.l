%{

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include "rml.h"
#include "parse-rdb.h"

int aarmldb_lineno = 1;

/* %option always-interactive */
%}


%option 8bit
%option nodefault
%option outfile = "scan-rdb.c"
%option prefix = "aarmldb"
%option noyywrap

LETTER  [A-Za-z_] 
DIGIT	[0-9]
ident   {LETTER}({LETTER}|{DIGIT}|"'"|"-"|".")*
tyvar   "'"{LETTER}({LETTER}|{DIGIT}|"-"|"'")*
file    ({LETTER}|{DIGIT}|"."|"\\"|"/"|"-")*

%x c_comment

%%

^"l:" {
	return T_VAL;
}

^"v:" {
	return T_VAR;
}

^"c:" {
	return T_CON;
}

^"t:" {
	return T_TYPE;
}

^"r:" {
	return T_RELATION;
}

"(" {
  return T_LPAR;
}

")" {
  return T_RPAR;
}

"," {
  return T_COMMA;
}

"[" {
  return T_LBRACK;
}

"]" {
  return T_RBRACK;
}

"*" {
  return T_STAR;
}

"=>" {
 return T_FATARROW;
 }
 
"." {
return T_DOT;
}

":" {
return T_COLON;
}

"|" {
return T_MORE;
}
 
{DIGIT}+ { 
	aarmldblval.vint = atoi(aarmldbtext);
	if (aarmldbdebug) fprintf(stderr, "FLEX:[NUMBER:%s at line %d]", aarmldbtext, aarmldb_lineno); 
	return T_number; 
}

{ident} { 
	aarmldblval.vid = (char*)malloc(strlen(aarmldbtext)+1);
	if (aarmldbdebug) fprintf(stderr, "FLEX:[ID:%s at line %d]", aarmldbtext, aarmldb_lineno); 
	strcpy(aarmldblval.vid, aarmldbtext); 
	return T_id; 
}

{tyvar} { 
	aarmldblval.vid = (char*)malloc(strlen(aarmldbtext)+1);
	if (aarmldbdebug) fprintf(stderr, "FLEX:[TYVAR:%s at line %d]", aarmldbtext, aarmldb_lineno); 
	strcpy(aarmldblval.vid, aarmldbtext);
	return T_tyvar; 
}

"<"{file}">" { 
	aarmldblval.vid = (char*)malloc(strlen(aarmldbtext)-1);
	if (aarmldbdebug) fprintf(stderr, "FLEX:[ID:%s at line %d]", aarmldbtext, aarmldb_lineno); 
	strncpy(aarmldblval.vid, aarmldbtext+1, strlen(aarmldbtext)-2);
	aarmldblval.vid[strlen(aarmldbtext)-2] = '\0'; 
	return T_file; 
}

"/\*"           {
                    BEGIN(c_comment);
                }

<c_comment>
{
    "\*/\r\n"|"\*/\n"|"\*/\r" {
                    BEGIN(INITIAL);
                }
    "/\*"       {
                     aarmldberror("Suspicious comment");
                }
    [^\n]       /* Skip stuff in comments */
	\r\n|\n|\r  { aarmldb_lineno++; }
    <<EOF>>     {
                     aarmldberror("Unterminated comment");
                     aarmldb_lineno = 1;
                     return T_EOF;
                 }
}
\r\n|\n|\r      { aarmldb_lineno++; return T_EOL; }
[ \t\r\010]+	{ /* Skip whitespace */ }
<<EOF>>			{  aarmldb_lineno = 1; return T_EOF; }
\/\/.*$         /* Skip single-line comment */
.				{ if (aarmldbdebug) fprintf(stderr,"FLEX:bad character: %s at line %d\n", aarmldbtext, aarmldb_lineno); }
%%


