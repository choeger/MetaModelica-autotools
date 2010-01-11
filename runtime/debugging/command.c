
#ifdef RML_DEBUG

#include <string.h>
#include <stdlib.h>
#include "rml.h"

rmldb_command_type* rmldb_current_command = NULL;

rmldb_command_type* rmldb_mk_cmd(int ty)
{
	rmldb_command_type* cmd = (rmldb_command_type*)malloc(sizeof(rmldb_command_type));
	memset(cmd, 0, sizeof(rmldb_command_type));
	cmd->ty = ty;
	return cmd;
}

rmldb_command_type* rmldb_mk_cmd_brkFile(int ty, char* file, char* line)
{
	rmldb_command_type* cmd = rmldb_mk_cmd(ty);
	cmd->data.brkFile.file = file;
	cmd->data.brkFile.line = atoi(line);
	return cmd;	
}

rmldb_command_type* rmldb_mk_cmd_brkFunction(int ty, char* file, char* name)
{
	rmldb_command_type* cmd = rmldb_mk_cmd(ty);
	cmd->data.brkFunction.file = file;
	cmd->data.brkFunction.name = name;	
	return cmd;		
}

rmldb_command_type* rmldb_mk_cmd_brkString(int ty, char* filter)
{
	rmldb_command_type* cmd = rmldb_mk_cmd(ty);
	cmd->data.brkString.filter = filter;
	return cmd;		
}

rmldb_command_type* rmldb_mk_cmd_brkFailure(int ty, char* filter)
{
    rmldb_command_type* cmd = rmldb_mk_cmd(ty);
    cmd->data.brkFailure.filter = filter;
    return cmd;     
}

rmldb_command_type* rmldb_mk_cmd_varValue(int ty, char* name)
{
	rmldb_command_type* cmd = rmldb_mk_cmd(ty);
	cmd->data.varValue.name = name;
	return cmd;		
}

rmldb_command_type* rmldb_mk_cmd_lazyVarValue(int ty, char* name, char* path)
{
	rmldb_command_type* cmd = rmldb_mk_cmd(ty);
	cmd->data.lazyVarValue.name = name;
	cmd->data.lazyVarValue.path = NULL; /* TODO add path here */	
	return cmd;		
}

rmldb_command_type* rmldb_mk_cmd_frameVarValue(int ty, char* frame, char* nr)
{
	rmldb_command_type* cmd = rmldb_mk_cmd(ty);
	cmd->data.frameVarValue.frame = atoi(frame);
	cmd->data.frameVarValue.nr = atoi(nr);	
	return cmd;		
}

rmldb_command_type* rmldb_mk_cmd_frameLazyVarValue(int ty, char* frame, char* nr, char* path)
{
	rmldb_command_type* cmd = rmldb_mk_cmd(ty);
	cmd->data.frameLazyVarValue.frame = atoi(frame);
	cmd->data.frameLazyVarValue.nr = atoi(nr);
	cmd->data.frameLazyVarValue.path = NULL;			
	return cmd;		
}

int rmldb_freeCommand(rmldb_command_type* cmd)
{
	if (!cmd) return RMLDB_SUCCESS;
	switch(cmd->ty)
	{	
	case RMLDB_CMD_QUIT:
	case RMLDB_CMD_CLEAR_BREAKPOINTS:
	case RMLDB_CMD_STACK:
	case RMLDB_CMD_STEP:
	case RMLDB_CMD_STEP_OVER:
	case RMLDB_CMD_STEP_RETURN:	
	case RMLDB_CMD_RUN:
	case RMLDB_CMD_FRAME_VARIABLE_VALUE:
		/* nothing to clear here, just break */
		break;
	case RMLDB_CMD_BREAKPOINT:
	case RMLDB_CMD_DELETE_BREAKPOINT:
		if (cmd->data.brkFile.file) free(cmd->data.brkFile.file);
		break;
	case RMLDB_CMD_FUNCTION_BREAKPOINT:
	case RMLDB_CMD_DELETE_FUNCTION_BREAKPOINT:
		if (cmd->data.brkFunction.file) free(cmd->data.brkFunction.file);
		if (cmd->data.brkFunction.name) free(cmd->data.brkFunction.name);			
		break;
	case RMLDB_CMD_STRING_BREAKPOINT:
	case RMLDB_CMD_DELETE_STRING_BREAKPOINT:
		if (cmd->data.brkString.filter) free(cmd->data.brkString.filter);	
		break;
    case RMLDB_CMD_FAILURE_BREAKPOINT:
    case RMLDB_CMD_DELETE_FAILURE_BREAKPOINT:
        if (cmd->data.brkFailure.filter) free(cmd->data.brkFailure.filter);   
        break;
	case RMLDB_CMD_VARIABLE_VALUE:
		if (cmd->data.varValue.name) free(cmd->data.varValue.name);	
		break;
	case RMLDB_CMD_LAZY_VARIABLE_VALUE:
		if (cmd->data.lazyVarValue.name) free(cmd->data.lazyVarValue.name);
		if (cmd->data.lazyVarValue.path) free(cmd->data.lazyVarValue.path);		
		break;
	case RMLDB_CMD_FRAME_LAZY_VARIABLE_VALUE:
		if (cmd->data.frameLazyVarValue.path) free(cmd->data.frameLazyVarValue.path);
		break;
	}	
	free(cmd);
	return RMLDB_SUCCESS;
}

#endif /* RML_DEBUG */
