%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "rml.h"

#define YYDEBUG 1 
extern int aadebug;
extern int aalex(void);

#define FALSE 0
#define TRUE  1

%}

/* The different semantic values that can be returned within the AST. */
%union
{
	char* id;
}

/* uncomment this shit on linux and comment it on solaris damn incompatibilities */
/* %name-prefix="aa" */

%token T_BREAKPOINT T_DELETE_BREAKPOINT T_CLEAR_BREAKPOINTS
%token T_FUNCTION_BREAKPOINT T_DELETE_FUNCTION_BREAKPOINT
%token T_STRING_BREAKPOINT T_DELETE_STRING_BREAKPOINT
%token T_FAILURE_BREAKPOINT T_DELETE_FAILURE_BREAKPOINT
%token T_STEP T_STEP_OVER T_STEP_RETURN T_RUN
%token T_STACK 
%token T_VARIABLE_VALUE T_LAZY_VARIABLE_VALUE
%token T_FRAME_VARIABLE_VALUE T_FRAME_LAZY_VARIABLE_VALUE
%token T_QUIT
%token T_COLON
%token T_EOL
%token <id>T_ID

%start command
%%

command:  
T_BREAKPOINT T_ID T_COLON T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkFile(RMLDB_CMD_BREAKPOINT, $2, $4));
 	YYACCEPT;
}
| T_DELETE_BREAKPOINT T_ID T_COLON T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkFile(RMLDB_CMD_DELETE_BREAKPOINT, $2, $4));
	YYACCEPT;
}
| T_FUNCTION_BREAKPOINT T_ID T_COLON T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkFunction(RMLDB_CMD_FUNCTION_BREAKPOINT, $2, $4));
 	YYACCEPT;
}
| T_DELETE_FUNCTION_BREAKPOINT T_ID T_COLON T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkFunction(RMLDB_CMD_DELETE_FUNCTION_BREAKPOINT, $2, $4));
	YYACCEPT;
}
| T_STRING_BREAKPOINT T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkString(RMLDB_CMD_STRING_BREAKPOINT, $2));
	YYACCEPT;
}
| T_DELETE_STRING_BREAKPOINT T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkString(RMLDB_CMD_DELETE_STRING_BREAKPOINT, $2));
	YYACCEPT;
}
| T_FAILURE_BREAKPOINT T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkFailure(RMLDB_CMD_FAILURE_BREAKPOINT, $2));
	YYACCEPT;
}
| T_DELETE_FAILURE_BREAKPOINT T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_brkFailure(RMLDB_CMD_DELETE_FAILURE_BREAKPOINT, $2));
	YYACCEPT;
}
| T_CLEAR_BREAKPOINTS T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd(RMLDB_CMD_CLEAR_BREAKPOINTS));
	YYACCEPT;
}
| T_STEP T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd(RMLDB_CMD_STEP));
	YYACCEPT;
}
| T_STEP_OVER T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd(RMLDB_CMD_STEP_OVER));
	YYACCEPT;
}
| T_STEP_RETURN T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd(RMLDB_CMD_STEP_RETURN));
	YYACCEPT;
}
| T_RUN T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd(RMLDB_CMD_RUN));
	YYACCEPT;
}
| T_STACK T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd(RMLDB_CMD_STACK));
	YYACCEPT;
}
| T_VARIABLE_VALUE T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_varValue(RMLDB_CMD_VARIABLE_VALUE, $2));
	YYACCEPT;
}
| T_LAZY_VARIABLE_VALUE T_ID T_COLON T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_lazyVarValue(RMLDB_CMD_LAZY_VARIABLE_VALUE, $2, $4));
	YYACCEPT;
}
| T_FRAME_VARIABLE_VALUE T_ID T_COLON T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_frameVarValue(RMLDB_CMD_FRAME_VARIABLE_VALUE, $2, $4));
	YYACCEPT;
}
| T_FRAME_LAZY_VARIABLE_VALUE T_ID T_COLON T_ID T_COLON T_ID T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd_frameLazyVarValue(RMLDB_CMD_DELETE_BREAKPOINT, $2, $4, $6));
	YYACCEPT;
}
| T_QUIT T_EOL
{
	rmldb_executeCommand(rmldb_mk_cmd(RMLDB_CMD_QUIT));
	YYACCEPT;
}
;

%%

int aaerror(char *s) 
{
  fprintf(stderr, "%s or command not recognized\n", s);
  return -1;
}

 