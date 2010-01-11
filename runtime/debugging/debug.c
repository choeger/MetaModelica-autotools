/* 
 * This file is part of OpenModelica.
 * 
 * It implements the Relational Meta-Language (RML) and MetaModelica
 * http://www.ida.liu.se/~pelab/rml
 * 
 * Copyright (c) 1998-2008, Linköpings University,
 * Department of Computer and Information Science, 
 * SE-58183 Linköping, Sweden. 
 * 
 * All rights reserved.
 * 
 * THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THIS OSMC PUBLIC 
 * LICENSE (OSMC-PL). ANY USE, REPRODUCTION OR DISTRIBUTION OF 
 * THIS PROGRAM CONSTITUTES RECIPIENT'S ACCEPTANCE OF THE OSMC 
 * PUBLIC LICENSE. 
 * 
 * The OpenModelica software and the Open Source Modelica 
 * Consortium (OSMC) Public License (OSMC-PL) are obtained 
 * from Linköpings University, either from the above address, 
 * from the URL: http://www.ida.liu.se/projects/OpenModelica
 * and in the OpenModelica distribution.
 * 
 * This program is distributed  WITHOUT ANY WARRANTY; without 
 * even the implied warranty of  MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH 
 * IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS 
 * OF OSMC-PL. 
 * 
 * See the full OSMC Public License conditions for more details.
 * 
 * @author Adrian Pop [adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo]
 * 
 */

/***********************************************************
[ debug.c ] 
- Adrian Pop, adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo 
- creation: 2002-10
  + implementation for rml debugging 
    this file contains all the functions for debugging.
  + all things in this file starts with rmldb_
- last modified: 2007-06-01
************************************************************/
/************************************************************/
/* all these functions depends on RML_DEBUG macro */
/**************************************************/

#include <stdarg.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#ifndef  _RML_DEBUG_H_
#include "rml.h"
#endif

#ifdef RML_DEBUG
/**************************************************/


#define RMLDB_TYPE_SUFFIX_NONE   0
#define RMLDB_TYPE_SUFFIX_LIST   1
#define RMLDB_TYPE_SUFFIX_OPTION 2
#define RMLDB_TYPE_SUFFIX_LVAR   3
#define RMLDB_TYPE_SUFFIX_VECTOR 4
#define RMLDB_TYPE_SUFFIX_ARRAY  5

#define RMLDB_MAX_BREAKPOINTS 100
/* the breakpoints set by the user  */
rmldb_command_type*  rmldb_breakpoints[RMLDB_MAX_BREAKPOINTS];
int   rmldb_number_of_breakpoints; /* of course, the number of filters. */

/* the active variables at the current execution point  */
#define RMLDB_MAX_VARS 500
rmldb_var_node_t*    rmldb_vars[RMLDB_MAX_VARS];
int   rmldb_number_of_vars; /* of course, the number active vars. */

typedef struct rmldb_stack_frame
{
   char* file;
   int   line;
   int   charStart;
   int   charEnd;
   char* function;
   char* call;
   rmldb_var_node_t* variables[RMLDB_MAX_VARS];
   int   variablesNr; /* the number of varsiables in this stack frame. */
   unsigned int   rmlStackPointer;     
} rmldb_stack_frame_t;

/* the rml stack explained  */
#define RMLDB_MAX_STACK        1000
rmldb_stack_frame_t*   rmldb_stack[RMLDB_MAX_STACK];
int   rmldb_stack_point; /* of course, the stack pointer. */


int   rmldb_execution_type         = RMLDB_STEP;
int   rmldb_execution_startup_type = RMLDB_STEP;

int  rmldb_depth_of_variable_print   = 15; /* the depth of the variable showing */
int  rmldb_max_string_variable_print = 60; /* how many characters we display in strings */

/* Local functions */
#define RMLDB_MAX_LINE 10000

/* function to work with the program database  */
extern void rmldb_print_type_info_id(char* name, FILE* out);
extern void rmldb_print_type_constructor(char* constructor_name, FILE* out);
extern void rmldb_print_type_constructors(char* type_name, FILE* out);
extern void rmldb_print_type(rmldb_type_t* type, int detailed, FILE* out);
extern void rmldb_print_type_list(rmldb_LISTty_t* list, int detailed, char* separator, FILE* out);
extern rmldb_type_db_t* rmldb_get_type_db(char* var_name);
extern rmldb_type_t* rmldb_get_type(char* var_name);
extern rmldb_var_db_t* rmldb_get_var(char* var_name);
extern char* rmldb_get_con_name(rmldb_type_t *type, int constructor);
extern rmldb_con_db_t* rml_get_con(rmldb_type_t *type, int constructor);
extern int rmldb_is_type_transparent(rmldb_type_t *type);
extern rmldb_str_t* rmldb_make_rmldb_str(char* name);
extern void rmldb_str_list_print(rmldb_str_list_t* list);
extern void rmldb_str_list_free(rmldb_str_list_t* list);
extern void rmldb_type_list_to_list(rmldb_LISTty_t* list, rmldb_str_list_t* lst);
extern void rmldb_type_to_list(rmldb_type_t *type, rmldb_str_list_t* list);
extern int rmldb_str_list_get_size(rmldb_str_list_t* list);
extern char* rmldb_str_list_get_index(rmldb_str_list_t* list, int index);
extern rmldb_LISTty_t* rmldb_get_type_components(rmldb_con_db_t* con_db);
extern rmldb_type_t* rmldb_get_type_component(rmldb_type_t* type, int constr, int index);
extern rmldb_type_t* rmldb_get_type_comp_from_con(rmldb_LISTty_t* type, int index);
extern void rmldb_var_show(void *p, rmldb_type_t* type, int depth);
extern void rmldb_var_send(void *p, rmldb_type_t* type, int depth, int structDepth);
extern void rmldb_type_list_to_sock(rmldb_LISTty_t* list, int detailed, char* separator);
extern void rmldb_type_to_sock(rmldb_type_t *type, int detailed);

/* current execution range */
rmldb_current_execution_loc_t rmldb_current_execution_loc;

int _rmldb_socket_out(char* msg)
{
	return rmldb_socket_out(rmldb_communicator.reply_sock, msg);
}

int _rmldb_socket_outln(char* msg)
{
	return rmldb_socket_outln(rmldb_communicator.reply_sock, msg);
}



/*
this function inits the debugger
*/
int rmldb_init(void)
{ 
	char process_id[20];	
	if (rml_debug_enabled)
	{		
		/* initalize stuff */
		rmldb_current_execution_loc.file = NULL;
		rmldb_current_execution_loc.relation = NULL;
		rmldb_current_execution_loc.goal = NULL;
		rmldb_current_execution_loc.range.sl = 0;
		rmldb_current_execution_loc.range.sc = 0;
		rmldb_current_execution_loc.range.el = 0;
		rmldb_current_execution_loc.range.ec = 0;
	
		/* parse for the first commands  */
		rmldb_execution_type = rmldb_execution_startup_type;

    // start the command handler only if we don't run in trace all mode
    if (rmldb_execution_type != RMLDB_TRACE_ALL)
    {
		  // start the command handler!
		  rmldb_startSignalHandler(NULL);
    }		
		/* rmldb_push_stack_frame("Main.mo", -1, -1, 1, 1, 1, 1, "Main.main", "start"); */
	}	
	return RMLDB_SUCCESS;
}

int rmldb_end(void)
{
#ifdef RML_DEBUG
	if (rml_debug_enabled)
	{
		/*
		fprintf(stderr, "%s - handing the control to the runtime for exit\n", RMLDB_PROMPT);
		*/
		rmldb_endSignalHandler();	
	}
#endif /* RML_DEBUG */
	return RMLDB_SUCCESS;
}

void rmldb_exit(int status)
{
	/* close all stuff first */
	rmldb_end();
	exit(status);
}


int rmldb_handle_breakpoint_request(rmldb_command_type* cmd)
{
	int i = 0;
	rmldb_command_type* newCmd = NULL;
	switch(cmd->ty)
	{			
		case RMLDB_CMD_CLEAR_BREAKPOINTS:
			for (i = 0; i < rmldb_number_of_breakpoints; i++)
				rmldb_freeCommand(rmldb_breakpoints[i]);
			rmldb_number_of_breakpoints = 0;
			/*
			fprintf(stderr, "%s Info! Breakpoints cleared.", RMLDB_PROMPT);
			*/
			break;
		case RMLDB_CMD_BREAKPOINT:
			newCmd = (rmldb_command_type*)malloc(sizeof(rmldb_command_type));
			newCmd->ty = cmd->ty;
			newCmd->data.brkFile.file = strdup(cmd->data.brkFile.file);
			newCmd->data.brkFile.line = cmd->data.brkFile.line;
			rmldb_breakpoints[rmldb_number_of_breakpoints++] = newCmd;
			break;
		case RMLDB_CMD_DELETE_BREAKPOINT:			
			for (i = 0; i < rmldb_number_of_breakpoints; i++)
				if (rmldb_breakpoints[i]->ty == RMLDB_CMD_BREAKPOINT &&
					strcmp(rmldb_breakpoints[i]->data.brkFile.file, cmd->data.brkFile.file) == 0 &&
					rmldb_breakpoints[i]->data.brkFile.line == cmd->data.brkFile.line)
						break;
			rmldb_freeCommand(rmldb_breakpoints[i]);
			rmldb_breakpoints[i] = rmldb_breakpoints[rmldb_number_of_breakpoints-1];
			rmldb_number_of_breakpoints = rmldb_number_of_breakpoints - 1;
			break; 
		case RMLDB_CMD_FUNCTION_BREAKPOINT:
			newCmd = (rmldb_command_type*)malloc(sizeof(rmldb_command_type));
			newCmd->ty = cmd->ty;
			newCmd->data.brkFunction.file = strdup(cmd->data.brkFunction.file);
			newCmd->data.brkFunction.name = strdup(cmd->data.brkFunction.name);
			rmldb_breakpoints[rmldb_number_of_breakpoints++] = newCmd;
			break;			
		case RMLDB_CMD_DELETE_FUNCTION_BREAKPOINT:
			for (i = 0; i < rmldb_number_of_breakpoints; i++)
				if (rmldb_breakpoints[i]->ty == RMLDB_CMD_FUNCTION_BREAKPOINT &&
					strcmp(rmldb_breakpoints[i]->data.brkFunction.file, cmd->data.brkFunction.file) == 0 &&
					strcmp(rmldb_breakpoints[i]->data.brkFunction.name, cmd->data.brkFunction.name) == 0)
						break;
			rmldb_freeCommand(rmldb_breakpoints[i]);
			rmldb_breakpoints[i] = rmldb_breakpoints[rmldb_number_of_breakpoints-1];
			rmldb_number_of_breakpoints = rmldb_number_of_breakpoints - 1;
			break; 			
		case RMLDB_CMD_STRING_BREAKPOINT:
			newCmd = (rmldb_command_type*)malloc(sizeof(rmldb_command_type));
			newCmd->ty = cmd->ty;
			newCmd->data.brkString.filter = strdup(cmd->data.brkString.filter);
			rmldb_breakpoints[rmldb_number_of_breakpoints++] = newCmd;
			break;					
		case RMLDB_CMD_DELETE_STRING_BREAKPOINT:
			for (i = 0; i < rmldb_number_of_breakpoints; i++)
				if (rmldb_breakpoints[i]->ty == RMLDB_CMD_STRING_BREAKPOINT &&
					strcmp(rmldb_breakpoints[i]->data.brkString.filter, cmd->data.brkString.filter) == 0)
						break;
			rmldb_freeCommand(rmldb_breakpoints[i]);
			rmldb_breakpoints[i] = rmldb_breakpoints[rmldb_number_of_breakpoints-1];
			rmldb_number_of_breakpoints = rmldb_number_of_breakpoints - 1;
			break;
        case RMLDB_CMD_FAILURE_BREAKPOINT:
            newCmd = (rmldb_command_type*)malloc(sizeof(rmldb_command_type));
            newCmd->ty = cmd->ty;
            newCmd->data.brkFailure.filter = strdup(cmd->data.brkFailure.filter);
            rmldb_breakpoints[rmldb_number_of_breakpoints++] = newCmd;
            break;                  
        case RMLDB_CMD_DELETE_FAILURE_BREAKPOINT:
            for (i = 0; i < rmldb_number_of_breakpoints; i++)
                if (rmldb_breakpoints[i]->ty == RMLDB_CMD_FAILURE_BREAKPOINT &&
                    strcmp(rmldb_breakpoints[i]->data.brkFailure.filter, cmd->data.brkFailure.filter) == 0)
                        break;
            rmldb_freeCommand(rmldb_breakpoints[i]);
            rmldb_breakpoints[i] = rmldb_breakpoints[rmldb_number_of_breakpoints-1];
            rmldb_number_of_breakpoints = rmldb_number_of_breakpoints - 1;
            break;
		/* if is not one of those, just return */
		default: return RMLDB_FAILURE;
	}
	return RMLDB_SUCCESS;
}


void* rmldb_get_var_value(int frameNr, int varNr)
{
	int i = 0; void* value = NULL;
	rmldb_stack_frame_t* frame = NULL;	
	/* if we ask for something odd, return null */
	if (frameNr >= rmldb_stack_point) return value;
	frame = rmldb_stack[frameNr];
	if (varNr > frame->variablesNr) return NULL;
	return frame->variables[varNr]->var;
}

char* rmldb_get_var_name(int frameNr, int varNr)
{
	int i = 0; void* value = NULL;
	rmldb_stack_frame_t* frame = NULL;	
	/* if we ask for something odd, return null */
	if (frameNr >= rmldb_stack_point) return value;
	frame = rmldb_stack[frameNr];
	if (varNr > frame->variablesNr) return NULL;
	return frame->variables[varNr]->var_name;
}

/* variables */
int rmdlb_handle_variable_request(rmldb_command_type* cmd)
{
	char* var_name = NULL; 
	rmldb_type_t* type=NULL; 
	void* value = NULL;	
	switch(cmd->ty)
	{
	case RMLDB_CMD_VARIABLE_VALUE:
	case RMLDB_CMD_LAZY_VARIABLE_VALUE:
		rmldb_sendReply("not implemented yet");
	case RMLDB_CMD_FRAME_VARIABLE_VALUE:
			value = rmldb_get_var_value(
				cmd->data.frameVarValue.frame,
				cmd->data.frameVarValue.nr);
			char* var_name = rmldb_get_var_name(
				cmd->data.frameVarValue.frame,
				cmd->data.frameVarValue.nr);
			if (var_name == NULL)
			{ 
				rmldb_sendReply("variable not found");
				break;
			}
			rmldb_type_t* type=rmldb_get_type(var_name);
			if (type == NULL) rmldb_sendReply("variable type not found");
			else
			{
                /* fprintf(stderr, "Sending type\n"); fflush(stderr); */
				rmldb_type_to_sock(type, 0);
				rmldb_sendReply("");
                /* fprintf(stderr, "Showing value\n"); fflush(stderr); 
                rmldb_var_show(value, type, 0); fflush(stdout); 
                fprintf(stderr, "Sending value\n"); fflush(stderr); */
				rmldb_var_send(value, type, 0, -1);
                /* fprintf(stderr, "Sending STOP\n"); fflush(stderr); */
				rmldb_sendReply("<.$STOP$.>");
			}
			break;
	case RMLDB_CMD_FRAME_LAZY_VARIABLE_VALUE:
		rmldb_sendReply("not implemented yet");	
		break;
	}
	return RMLDB_SUCCESS;
}

int rmldb_push_stack_frame(
			char* file, 
			int charStart, int charEnd,
			int startLine, int startColumn, int endLine, int endColumn,					  
			char* function,
			char* call)
{
    int i = rmldb_stack_point-1; /* take the last frame */
#if	defined(RML_STATE_APTR) || defined(RML_STATE_LPTR)
    struct rml_state *rmlState = &rml_state;
#endif	/*RML_STATE_APTR || RML_STATE_LPTR*/
    /* if the previous push type was 'e'xternal, and this one is in the same module, pop the previous record */
    if (i >= 0 &&
        strcmp(rmldb_stack[i]->call, "e") == 0 &&
        strcmp(rmldb_stack[i]->file, file) == 0 && /* same file */
        strcmp(rmldb_stack[i]->function, function) == 0) /* same function */
    {
        rmldb_pop_stack_frame('u');
    }
    else if (i >= 0 &&
        strcmp(call, "n") == 0 &&
        strcmp(rmldb_stack[i]->call, "n") == 0 
        )
    {
        rmldb_pop_stack_frame('u');
    }
    
	{		
		rmldb_stack_frame_t* newFrame = (rmldb_stack_frame_t*)malloc(sizeof(rmldb_stack_frame_t));
		newFrame->file = file; 
		newFrame->line = startLine;
		newFrame->charStart = charStart;
		newFrame->charEnd = charEnd;				
		newFrame->function = function; 
		newFrame->call = call; 
		newFrame->variablesNr = rmldb_number_of_vars;
		newFrame->rmlStackPointer = &rml_stack[rml_stack_size]-(void**)rmlSP;
		for (i = 0; i < rmldb_number_of_vars; i++)
		{
			rmldb_var_node_t* variable = (rmldb_var_node_t*)malloc(sizeof(rmldb_var_node_t));
			variable->var_name  = strdup(rmldb_vars[i]->var_name);
			variable->var   	= rmldb_vars[i]->var;
			variable->depth 	= i;
			variable->direction = rmldb_vars[i]->direction;
			newFrame->variables[i] = variable;
		}	 
		newFrame->variablesNr = rmldb_number_of_vars;
		if (rmldb_stack_point >= RMLDB_MAX_STACK-1)
		{
			/* clear the stack if is full! */
			while (rmldb_stack_point != 0) rmldb_pop_stack_frame('s');
			rmldb_stack[rmldb_stack_point++] = newFrame;
		}
		else	
			rmldb_stack[rmldb_stack_point++] = newFrame;
	}
	return RMLDB_SUCCESS;
}

extern int rmldb_pop_stack_frame(char gototype)
{
	int i = 0;
	rmldb_stack_frame_t* tmp1 = NULL;
    rmldb_stack_frame_t* tmp2 = NULL;
	if (rmldb_stack_point == 0) return RMLDB_SUCCESS;

    switch(gototype)
    {
        case 'n':
        case 'e':
            return RMLDB_SUCCESS;
                        
        case 's':
        case 'f':
            if (rmldb_stack_point-2 >= 0)
            {
                tmp2 = rmldb_stack[rmldb_stack_point-2];
                tmp1 = rmldb_stack[rmldb_stack_point-1];
                if (!strcmp(tmp2->call, "n") /* &&
                    !strcmp(tmp1->function, tmp2->function) &&
                    !strcmp(tmp1->file, tmp2->file)*/)
                {
                    /* pop also the normal call */
                    tmp1 = rmldb_stack[rmldb_stack_point-1];    
                    for (i = 0; i < tmp1->variablesNr; i++)
                    {
                        free(tmp1->variables[i]->var_name);
                        free(tmp1->variables[i]);
                    }
                    free(rmldb_stack[rmldb_stack_point-1]);
                    rmldb_stack_point--;                    
                }
            }
            break;
        case 'h':
        case 'u':                
            break;
    }
    
	tmp1 = rmldb_stack[rmldb_stack_point-1];	
	for (i = 0; i < tmp1->variablesNr; i++)
	{
		free(tmp1->variables[i]->var_name);
		free(tmp1->variables[i]);
	}
	free(rmldb_stack[rmldb_stack_point-1]);
	rmldb_stack_point--;
}


/* stack */
int rmdlb_handle_stack_request(rmldb_command_type* cmd)
{
	/* send the stack contents to the UI */
	/* the UI expects this:
	 * "Main.mo|line|startChar|endChar|stackPointer|main|arg1|arg2|arg3#nextFrame";
	 * file|line|function|arg1|arg2|arg3 
	 */
	 int i = 0, j=0; char buf[200];
	 for (i = 0; i < rmldb_stack_point; i++)
	 {
	 	rmldb_socket_out(rmldb_communicator.reply_sock, rmldb_stack[i]->file);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, "|");	 	
	 	sprintf(buf, "%d", rmldb_stack[i]->line);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, buf);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, "|");
	 	sprintf(buf, "%d", rmldb_stack[i]->charStart);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, buf);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, "|");
	 	sprintf(buf, "%d", rmldb_stack[i]->charEnd);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, buf);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, "|");	 		 		 	
	 	rmldb_socket_out(rmldb_communicator.reply_sock, rmldb_stack[i]->function);
	 	rmldb_socket_out(rmldb_communicator.reply_sock, "|");
	 	sprintf(buf, "%d", rmldb_stack[i]->rmlStackPointer);	
	 	rmldb_socket_out(rmldb_communicator.reply_sock, buf);
        rmldb_socket_out(rmldb_communicator.reply_sock, "|");
        sprintf(buf, "%s", rmldb_stack[i]->call);    
        rmldb_socket_out(rmldb_communicator.reply_sock, buf);        
	 	rmldb_socket_out(rmldb_communicator.reply_sock, "|");	 		 	
	 	for (j=0; j < rmldb_stack[i]->variablesNr; j++)
	 	{
	 		rmldb_socket_out(rmldb_communicator.reply_sock, rmldb_stack[i]->variables[j]->var_name);
	 		if (j < rmldb_stack[i]->variablesNr-1) rmldb_socket_out(rmldb_communicator.reply_sock, "|");	 		
	 	}
	 	if (i < rmldb_stack_point-1)
	 		rmldb_socket_out(rmldb_communicator.reply_sock, "#");
	 }
	 rmldb_socket_out(rmldb_communicator.reply_sock, "\n");	
}


void rmldb_pr_ident(unsigned long depth)
{
	unsigned long i;
	for (i = 0; i < depth; i++) printf(" ");
}

int rmldb_add_active_var(int direction, char* var_name, void* var)
{
	int i = 0;
	if (rml_debug_enabled)
	{	
		rmldb_var_node_t* variable = NULL;
		/* search for already existing ones! */	
		for (i = 0; i < rmldb_number_of_vars; i++)
		{
			if (strcmp(rmldb_vars[i]->var_name, var_name) == 0 &&
			    rmldb_vars[i]->var == var)
				return RMLDB_SUCCESS;
		}
		variable = (rmldb_var_node_t*)malloc(sizeof(rmldb_var_node_t));
		variable->var_name  = strdup(var_name);
		variable->var   	= var;
		variable->depth 	= rmldb_number_of_vars;
		variable->direction = direction;
		if (rmldb_number_of_vars >= RMLDB_MAX_VARS-1)
		{
			return RMLDB_FAILURE;
		}
		else
		{
			rmldb_vars[rmldb_number_of_vars] = variable;
		}
		rmldb_number_of_vars = rmldb_number_of_vars + 1;
	}
	return RMLDB_SUCCESS;
}

int rmldb_clear_active_vars(void)
{
	int i = 0;
	if (rml_debug_enabled)
	{	
		for (i = 0; i < rmldb_number_of_vars; i++) free(rmldb_vars[i]);
		rmldb_number_of_vars = 0;
	}
	return RMLDB_SUCCESS;
}


rmldb_str_t* rmldb_make_rmldb_str(char* name)
{
	rmldb_str_t* rmldb_str_node = (rmldb_str_t*)malloc(sizeof(rmldb_str_t));
	rmldb_str_node->name = name;
	rmldb_str_node->depth = 0;
	rmldb_str_node->next = NULL;
	rmldb_str_node->prev = NULL;
	return rmldb_str_node;
}

void rmldb_str_list_add(
					rmldb_str_t **start_node, 
				    rmldb_str_t **end_node, 
					rmldb_str_t *node)
{
	rmldb_str_t *last;
	last = node;
	if(*start_node)
		last->depth = ((rmldb_str_t*)*end_node)->depth + 1;
	else { *start_node = last; last->depth = 0; }
	last->next = NULL;	last->prev = *end_node;
	if(*end_node) ((rmldb_str_t*)*end_node)->next = last;
	*end_node = last;
}

void rmldb_str_list_print(rmldb_str_list_t* list)
{
	rmldb_str_t* tmp;
	fprintf(stderr, "=TYPE_LIST(");
	if (list) 
		for (tmp = list->list_start; tmp; tmp = tmp->next)
		{
			if (tmp->next) fprintf(stderr, "%s, ", tmp->name);
			else fprintf(stderr, "%s", tmp->name);
		}
	fprintf(stderr, ")\n");
}

char* rmldb_get_relation_type()
{
	char* name = rmldb_stack[rmldb_stack_point-1]->function;
	int detailed = 1;
	rmldb_type_t * type = NULL;
	rmldb_relation_db_t* tmpr;	
	for(tmpr = rmldb_relation_db_start; tmpr; tmpr = tmpr->next)
	{
		if (strcmp(name, tmpr->name) == 0)
		{
			type = tmpr->type_db->type;
			if (!type) { fprintf(stdout,"()"); return; } 
			switch (type->kind)
			{
			case RMLDB_eLISTty:  
				{
					if (detailed) fprintf(stdout, "LISTty(["); 
					else fprintf(stdout, "[");
					rmldb_print_type_list(type->component.l, detailed, ", ", stdout);
					if (detailed) fprintf(stdout, "])"); else fprintf(stdout, "]");
					break;
				}
			case RMLDB_eVARty:   
				{
					if (detailed) fprintf(stdout, "VARty(%s)", type->component.v->id);
					else fprintf(stdout, "%s", type->component.v->id);
					break;
				}
			case RMLDB_eCONSty:  
				{
					if (detailed) fprintf(stdout, "CONSty([");
					rmldb_print_type_list(type->component.c->list, detailed, ", " , stdout); 
					if (detailed) fprintf(stdout, "], %s)", type->component.c->id);
					else if (type->component.c->list && type->component.c->list->list_start) 
							fprintf(stdout, " %s", type->component.c->id);
					else fprintf(stdout, "%s", type->component.c->id);
					break;
				}
			case RMLDB_eTUPLEty: 
				{
					if (detailed) fprintf(stdout, "TUPLEty([");
					else fprintf(stdout, "(");
					rmldb_print_type_list(type->component.t->list, detailed, " * ", stdout);
					if (detailed) fprintf(stdout, "])");
					else fprintf(stdout, ")");
					break;
				}
			case RMLDB_eRELty:   
				{
					if (detailed) fprintf(stdout, "RELty(["); else fprintf(stdout, "(("); 
					rmldb_print_type_list(type->component.r->list1, detailed, ", ",  stdout);
					if (detailed) fprintf(stdout, "] => ["); else fprintf(stdout, ") => (");
					rmldb_print_type_list(type->component.r->list2, detailed, ", ", stdout);
					if (detailed) fprintf(stdout, "])"); else fprintf(stdout, "))");
					break;
				}
			}
			break;
		}
	}
	return "";
}
			

void rmldb_print_type_info_id(char* id, FILE* out)
{
	rmldb_relation_db_t* tmpr;	rmldb_type_db_t* tmpt;
	rmldb_con_db_t* tmpc;  	rmldb_var_db_t* tmpd;
	if (!id) { fprintf(out, "wrong id\n"); return; }
	/* search in relations, vars, types */
	fprintf(out, "*********** TYPE INFO ON ID: %s *************\n", id);
	/* relations */
	fprintf(out, "- relations");
	for(tmpr = rmldb_relation_db_start; tmpr; tmpr = tmpr->next)
	{
    /* printf("searching relation: %s \n", tmpr->name); */
		if (strcmp(id, tmpr->name) == 0)
		{
			fprintf(out, "\n\t+ %s:%d.%d.%d.%d|relation:%s|type:",
				tmpr->file,
				tmpr->range->sl,tmpr->range->sc,tmpr->range->el,tmpr->range->el,
				tmpr->name);
			rmldb_print_type(tmpr->type_db->type, 0, out);
		}
	}
	/* types */
	fprintf(out, "\n- types");
	for(tmpt = rmldb_type_db_start; tmpt; tmpt = tmpt->next)
	{
		/* rmldb_current_execution_loc must be included in type range -> type */
    /* printf("searching type: %s \n", tmpt->name); */
		if (strcmp(id, tmpt->name) == 0)
		{
			fprintf(out, "\n\t+ %s:%d.%d.%d.%d|type:%s|constructors:\n",
				tmpt->file,
				tmpt->range->sl,tmpt->range->sc,tmpt->range->el,tmpt->range->el,
				tmpt->name,
				id);
			rmldb_print_type_constructors(tmpt->name, out);
		}
	}
	/* constructors */
	fprintf(out, "\n- type constructors");
	for(tmpc = rmldb_con_db_start; tmpc; tmpc = tmpc->next)
	{
    /* printf("searching constructors: %s \n", tmpc->name); */
		if (strcmp(id, tmpc->name) == 0)
		{
			fprintf(out, "\n\t+ %s:%d.%d.%d.%d|constructor:",
				tmpc->file,
				tmpc->range->sl,tmpc->range->sc,tmpc->range->el,tmpc->range->el);
			rmldb_print_type_constructor(tmpc->name, out);
			fprintf(out,"\n");
		}
	}
	/* vars */
	fprintf(out, "\n- variables");
	rmldb_var_db_t* tmpv;
	for(tmpv = rmldb_var_db_start; tmpv; tmpv = tmpv->next)
	{
    /* printf("searching variables: %s \n", tmpv->name); */
		if (strcmp(id, tmpv->name) == 0)
		{
			fprintf(out, "\n\t+ %s:%d.%d.%d.%d|relation: %s|clause range: %d.%d.%d.%d|variable: %s has type:",
				tmpv->file,
				tmpv->range->sl,tmpv->range->sc,tmpv->range->el,tmpv->range->el,
				tmpv->relation,
				tmpv->clause_range->sl,tmpv->clause_range->sc,tmpv->clause_range->el,tmpv->clause_range->el,
				id);
			rmldb_print_type(tmpv->type_db->type, 0, out);
		}
	}
	printf("\n*************************************************************\n");
}

void rmldb_type_list_to_list(rmldb_LISTty_t* list, rmldb_str_list_t* lst)
{
	rmldb_type_t* tmp;
	for (tmp = list->list_start; tmp; tmp = tmp->next)
		rmldb_type_to_list(tmp, lst);
}

void rmldb_type_to_list(rmldb_type_t *type, rmldb_str_list_t* list)
{
	if (!type) { return; } 
	switch (type->kind)
	{
	case RMLDB_eLISTty:  
		{
			rmldb_type_list_to_list(type->component.l, list);
			break;
		}
	case RMLDB_eVARty:   
		{
			rmldb_str_list_add(
				&list->list_start,
				&list->list_end,
				rmldb_make_rmldb_str(type->component.v->id)); 
			break;
		}
	case RMLDB_eCONSty:  
		{ 
			rmldb_type_list_to_list(type->component.c->list, list);
			rmldb_str_list_add(
				&list->list_start,
				&list->list_end,
				rmldb_make_rmldb_str(type->component.c->id));
			break;
		}
	case RMLDB_eTUPLEty: 
		{ 
			rmldb_type_list_to_list(type->component.t->list, list);
			break;
		}
	case RMLDB_eRELty:   
		{ 
			rmldb_type_list_to_list(type->component.r->list1, list);
			rmldb_type_list_to_list(type->component.r->list2, list);			
			break;
		}
	}
}

void rmldb_print_type_list(rmldb_LISTty_t* list, int detailed, char* separator, FILE* out)
{
	rmldb_type_t* tmp;
	if (!list) { fprintf(out, "()"); return; }
	for (tmp = list->list_start; tmp; tmp = tmp->next)
		if (tmp->next) { rmldb_print_type(tmp, detailed, out); fprintf(out, "%s", separator); }
		else rmldb_print_type(tmp, detailed, out);
}

void rmldb_print_type(rmldb_type_t *type, int detailed, FILE* out)
{
	if (!type) { fprintf(out,"()"); return; } 
	switch (type->kind)
	{
	case RMLDB_eLISTty:  
		{
			if (detailed) fprintf(out, "LISTty(["); 
			else fprintf(out, "[");
			rmldb_print_type_list(type->component.l, detailed, ", ", out);
			if (detailed) fprintf(out, "])"); else fprintf(out, "]");
			break;
		}
	case RMLDB_eVARty:   
		{
			if (detailed) fprintf(out, "VARty(%s)", type->component.v->id);
			else fprintf(out, "%s", type->component.v->id);
			break;
		}
	case RMLDB_eCONSty:  
		{
			if (detailed) fprintf(out, "CONSty([");
			rmldb_print_type_list(type->component.c->list, detailed, ", " , out); 
			if (detailed) fprintf(out, "], %s)", type->component.c->id);
			else if (type->component.c->list && type->component.c->list->list_start) 
					fprintf(out, " %s", type->component.c->id);
			else fprintf(out, "%s", type->component.c->id);
			break;
		}
	case RMLDB_eTUPLEty: 
		{
			if (detailed) fprintf(out, "TUPLEty([");
			else fprintf(out, "(");
			rmldb_print_type_list(type->component.t->list, detailed, " * ", out);
			if (detailed) fprintf(out, "])");
			else fprintf(out, ")");
			break;
		}
	case RMLDB_eRELty:   
		{
			if (detailed) fprintf(out, "RELty(["); else fprintf(out, "(("); 
			rmldb_print_type_list(type->component.r->list1, detailed, ", ",  out);
			if (detailed) fprintf(out, "] => ["); else fprintf(out, ") => (");
			rmldb_print_type_list(type->component.r->list2, detailed, ", ", out);
			if (detailed) fprintf(out, "])"); else fprintf(out, "))");
			break;
		}
	}
}


void rmldb_type_list_to_sock(rmldb_LISTty_t* list, int detailed, char* separator)
{
	char buf[RMLDB_MAX_LINE];
	rmldb_type_t* tmp;
	if (!list) 
	{ 
		snprintf(buf, RMLDB_MAX_LINE, "()");
		_rmldb_socket_out(buf);
		return; 
	}
	for (tmp = list->list_start; tmp; tmp = tmp->next)
		if (tmp->next) 
		{ 
			rmldb_type_to_sock(tmp, detailed); 
			snprintf(buf, RMLDB_MAX_LINE, "%s", separator);
			_rmldb_socket_out(buf);
		}
		else rmldb_type_to_sock(tmp, detailed);
}

void rmldb_type_to_sock(rmldb_type_t *type, int detailed)
{
	char buf[RMLDB_MAX_LINE];
	if (!type) 
	{ 
		snprintf(buf, RMLDB_MAX_LINE, "()");
		_rmldb_socket_out(buf);
		return; 
	}
    /*
    if (type->name != NULL)
    {
        snprintf(buf, RMLDB_MAX_LINE,  "%s:(", type->name);
        _rmldb_socket_out(buf);
    } 
    */
	switch (type->kind)
	{
	case RMLDB_eLISTty:  
		{
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "LISTty(["); _rmldb_socket_out(buf);
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "["); _rmldb_socket_out(buf);
			}
			rmldb_type_list_to_sock(type->component.l, detailed, ", ");
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "])"); _rmldb_socket_out(buf);
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "]"); _rmldb_socket_out(buf);
			}
			break;
		}
	case RMLDB_eVARty:   
		{
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE, "VARty(%s)", type->component.v->id);
				_rmldb_socket_out(buf);
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "%s", type->component.v->id);
				_rmldb_socket_out(buf);
			}
			break;
		}
	case RMLDB_eCONSty:  
		{
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "CONSty([");
				_rmldb_socket_out(buf);
			}
			rmldb_type_list_to_sock(type->component.c->list, detailed, ", "); 
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "], %s)", type->component.c->id);
				_rmldb_socket_out(buf);
			}
			else if (type->component.c->list && type->component.c->list->list_start)
			{
				snprintf(buf, RMLDB_MAX_LINE,  " %s", type->component.c->id);
				_rmldb_socket_out(buf);
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "%s", type->component.c->id);
				_rmldb_socket_out(buf);
			}
			break;
		}
	case RMLDB_eTUPLEty: 
		{
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "TUPLEty(["); _rmldb_socket_out(buf);
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "("); _rmldb_socket_out(buf);
			}
			rmldb_type_list_to_sock(type->component.t->list, detailed, " * ");
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "])"); _rmldb_socket_out(buf);
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  ")"); _rmldb_socket_out(buf);
			}
			break;
		}
	case RMLDB_eRELty:   
		{
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "RELty(["); _rmldb_socket_out(buf); 
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "(("); _rmldb_socket_out(buf);
			}
			rmldb_type_list_to_sock(type->component.r->list1, detailed, ", ");
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE,  "] => ["); _rmldb_socket_out(buf);
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE,  ") => ("); _rmldb_socket_out(buf);
 			}
			rmldb_type_list_to_sock(type->component.r->list2, detailed, ", ");
			if (detailed) 
			{
				snprintf(buf, RMLDB_MAX_LINE, "])"); _rmldb_socket_out(buf); 
			}
			else 
			{
				snprintf(buf, RMLDB_MAX_LINE, "))"); _rmldb_socket_out(buf);
			}
			break;
		}
	}
    /*
    if (type->name != NULL)
    {
        snprintf(buf, RMLDB_MAX_LINE,  ")");
        _rmldb_socket_out(buf);
    }
    */    
}

#define RMLDB_RANGE_1IN2 2
#define RMLDB_RANGE_2IN1 -2
/* returns
  0 if equal
 +1 if [overlapping] bigger
 -1 if [overlapping] smaller
 +2 if r1 included in r2
 -2 if r2 included in r1
*/

int rmldb_range_incl(rmldb_range_db_t *r1, rmldb_range_db_t *r2)
{
	int factor = 1000;
	int t1s,t1e,t2s,t2e;
	t1s = r1->sl*factor + r1->sc; t1e = r1->el*factor + r1->ec; 
	t2s = r2->sl*factor + r2->sc; t2e = r2->el*factor + r2->ec;
	if (t1s >= t2s && t1e <= t2e) return RMLDB_RANGE_1IN2;
	if (t1s <= t2s && t1e >= t2e) return RMLDB_RANGE_2IN1;
	return rmldb_range_cmp(r1,r2);
}


rmldb_type_db_t* rmldb_get_type_db(char* var_name)
{
	rmldb_var_db_t* tmp; rmldb_relation_db_t* tmpr; rmldb_con_db_t* tmpc;
	for(tmp = rmldb_var_db_start; tmp; tmp = tmp->next)
	{
		/* rmldb_current_execution_loc must be included in variable range -> type */
		if (strcmp(rmldb_current_execution_loc.file, tmp->file) == 0) 
		if (strcmp(strstr(rmldb_current_execution_loc.relation, ".")+1, tmp->relation) == 0)
		if (strcmp(var_name, tmp->name) == 0)
		{
			if (rmldb_range_incl(&rmldb_current_execution_loc.range, tmp->clause_range) == RMLDB_RANGE_1IN2)
			{
				return tmp->type_db;
			}
			return tmp->type_db;
		}		
	}
	for(tmpr = rmldb_relation_db_start; tmpr; tmpr = tmpr->next)
	{
		if (strcmp(var_name, tmpr->name) == 0 ||
			strcmp(var_name, (strstr(tmpr->name,".")+1)) == 0)
		{
			return tmpr->type_db;
		}
	}
	for(tmpc = rmldb_con_db_start; tmpc; tmpc = tmpc->next)
	{
    /* printf("searching constructors: %s \n", tmpc->name); */
		if (strcmp(var_name, tmpc->name) == 0)
		{			
			return tmpc->type_db;
		}
	}	
	return NULL;
}

rmldb_type_t* rmldb_get_type(char* var_name)
{
	rmldb_var_db_t* tmp; rmldb_relation_db_t* tmpr; rmldb_con_db_t* tmpc;
	for(tmp = rmldb_var_db_start; tmp; tmp = tmp->next)
	{
		if (strcmp(rmldb_current_execution_loc.file, tmp->file) == 0) 
		if (strcmp(strstr(rmldb_current_execution_loc.relation, ".")+1, tmp->relation) == 0)
		if (strcmp(var_name, tmp->name) == 0)
		{
			if (rmldb_range_incl(&rmldb_current_execution_loc.range, tmp->clause_range) == RMLDB_RANGE_1IN2)
			{
				return tmp->type_db->type;
			}
		}		
	}
	for(tmp = rmldb_var_db_start; tmp; tmp = tmp->next)
	{
		if (strcmp(rmldb_current_execution_loc.file, tmp->file) == 0) 
		if (strcmp(strstr(rmldb_current_execution_loc.relation, ".")+1, tmp->relation) == 0)
		if (strcmp(var_name, tmp->name) == 0)
		{
			if (rmldb_range_incl(&rmldb_current_execution_loc.range, tmp->clause_range) == RMLDB_RANGE_1IN2)
				return tmp->type_db->type;
		}		
	}
	for(tmpr = rmldb_relation_db_start; tmpr; tmpr = tmpr->next)
	{
		if (strcmp(var_name, tmpr->name) == 0 ||
			strcmp(var_name, (strstr(tmpr->name,".")+1)) == 0)
		{
			return tmpr->type_db->type;
		}
	}
	for(tmpc = rmldb_con_db_start; tmpc; tmpc = tmpc->next)
	{
    /* printf("searching constructors: %s \n", tmpc->name); */
		if (strcmp(var_name, tmpc->name) == 0)
		{	
			if (tmpc->type_db->type->kind == RMLDB_eRELty)
				return tmpc->type_db->type->component.r->list2->list_start;
			return tmpc->type_db->type;
		}
	}	
	return NULL;
}


rmldb_var_db_t* rmldb_get_var(char* var_name)
{
	rmldb_var_db_t* tmp; rmldb_relation_db_t* tmpr;
	for(tmp = rmldb_var_db_start; tmp; tmp = tmp->next)
	{
		/* rmldb_current_execution_loc must be included in variable range -> type */
		if (strcmp(rmldb_current_execution_loc.file, tmp->file) == 0) 
		if (strcmp(rmldb_current_execution_loc.relation, tmp->relation) == 0)
		if (strcmp(var_name, tmp->name) == 0)
		if (rmldb_range_incl(&rmldb_current_execution_loc.range, tmp->clause_range) == RMLDB_RANGE_1IN2)
			return tmp;
	}
	return NULL;
}


char* rmldb_get_type_id(rmldb_con_db_t* con_db)
{
	if (con_db->type_db->type->kind == RMLDB_eRELty)
		return con_db->type_db->type->component.r->list2->list_start->component.c->id;
	if (con_db->type_db->type->kind == RMLDB_eCONSty)
		return con_db->type_db->type->component.c->id;
	fprintf(stderr, "ERROR, unexpected type kind %d", con_db->type_db->type->kind);
	return NULL;
}

rmldb_LISTty_t* rmldb_get_type_components(rmldb_con_db_t* con_db)
{
	if (con_db->type_db->type->kind == RMLDB_eRELty)
		return con_db->type_db->type->component.r->list1;
	if (con_db->type_db->type->kind == RMLDB_eCONSty)
		return NULL;
	fprintf(stderr, "ERROR, unexpected type kind %d", con_db->type_db->type->kind);
	return NULL;
}

void rmldb_print_type_constructors(char* type_name, FILE* out)
{
	rmldb_con_db_t *i;
	for(i = rmldb_con_db_start; i; i = i->next)
	{
		rmldb_str_list_t* components;
		components = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
		components->list_start = components->list_end = NULL;
		rmldb_type_to_list(i->type_db->type, components);
		/* 
		rmldb_print_type(i->type_db->type, 1, stderr);
		rmldb_str_list_print(components); 
		*/
		if (components && 
			components->list_start && 
			components->list_end->name)
		{
			if (!strcmp(type_name,
				components->list_end->name))
			{
				fprintf(out, "%s of ", i->name);
				rmldb_print_type_list(rmldb_get_type_components(i), 0, " * ", out);
				fprintf(out, "\n");
			}
		}
		rmldb_str_list_free(components);
	}
}

void rmldb_print_type_constructor(char* constructor_name, FILE* out)
{
	rmldb_con_db_t *i;
	for(i = rmldb_con_db_start; i; i = i->next)
	{
		if (!strcmp(constructor_name, i->name))
		{
			fprintf(out, "%s of ", i->name);
			rmldb_print_type_list(rmldb_get_type_components(i), 0, " * ", out);
			fprintf(out, " => %s", rmldb_get_type_id(i));
		}
	}
}

char* rmldb_get_con_name(rmldb_type_t *type, int constructor)
{
	rmldb_con_db_t *i;
	if (type)
	{
		rmldb_str_list_t* types;
		types = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
		types->list_start = types->list_end = NULL;
		rmldb_type_to_list(type, types);
		/* 
		rmldb_print_type(type, 0, stderr);
		rmldb_str_list_print(types);
		*/
		if (types && types->list_start && types->list_start->name)
		{
			for(i = rmldb_con_db_start; i; i = i->next)
			{
				rmldb_str_list_t* components;
				components = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
				components->list_start = components->list_end = NULL;
				rmldb_type_to_list(i->type_db->type, components);
				/* 
				rmldb_print_type(i->type_db->type, 0, stderr);
				rmldb_str_list_print(components); 
				*/
				if (components && 
					components->list_start && 
					types->list_start->name &&
					components->list_end->name)
				{
					if (!strcmp(types->list_start->name,
						components->list_end->name))
					{
						if (i->constructor == constructor)
							return i->name;
						/*
						if (i->is_transparent)
						{
							return i->name;
						}
						*/
					}
				}
				rmldb_str_list_free(components);
			}
		}
		rmldb_str_list_free(types);
	}
	return NULL;
}

rmldb_con_db_t* rmldb_get_con(rmldb_type_t *type, int constructor)
{
	rmldb_con_db_t *i;
	if (type)
	{
		rmldb_str_list_t* types;
		types = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
		types->list_start = types->list_end = NULL;
		rmldb_type_to_list(type, types);
		/* 
		rmldb_print_type(type, 0, stderr);
		rmldb_str_list_print(types);
		*/
		if (types && types->list_start && types->list_start->name)
		{
			for(i = rmldb_con_db_start; i; i = i->next)
			{
				rmldb_str_list_t* components;
				components = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
				components->list_start = components->list_end = NULL;
				rmldb_type_to_list(i->type_db->type, components);
				/* 
				rmldb_print_type(i->type_db->type, 0, stderr);
				rmldb_str_list_print(components); 
				*/
				if (components && 
					components->list_start && 
					types->list_start->name &&
					components->list_end->name)
				{
					if (!strcmp(types->list_start->name,
						components->list_end->name))
					{
						if (i->constructor == constructor)
							return i;
						/*
						if (i->is_transparent)
						{
							return i->name;
						}
						*/
					}
				}
				rmldb_str_list_free(components);
			}
		}
		rmldb_str_list_free(types);
	}
	return NULL;
}


rmldb_type_t* rmldb_get_type_comp_from_con(rmldb_LISTty_t* list, int index)
{
	rmldb_type_t* tmp;
	if (list) 
		for (tmp = list->list_start; tmp; tmp = tmp->next)
		{
			/*
			uncomment to debug
			fprintf(stderr, "\nINDEX=[%d]-DEPTH=[/%d]->TYPE:", index, tmp->depth); 
			rmldb_print_type(tmp, 0, stderr);
			*/
			if (tmp->depth == index) return tmp;
		}
	return NULL;
}

rmldb_type_t* rmldb_get_type_component(rmldb_type_t *type, int constructor, int index)
{
	rmldb_con_db_t *i;
	if (type)
	{
		rmldb_str_list_t* types;
		types = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
		types->list_start = types->list_end = NULL;
		rmldb_type_to_list(type, types);
		/*
		rmldb_print_type(type, 0, stderr);
		rmldb_str_list_print(types);
		*/
		if (types && types->list_start, types->list_end->name)
		{
			for(i = rmldb_con_db_start; i; i = i->next)
			{
				rmldb_str_list_t* components;
				components = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
				components->list_start = components->list_end = NULL;
				rmldb_type_to_list(i->type_db->type, components);
				/*
				rmldb_print_type(i->type_db->type, 0, stderr);
				rmldb_str_list_print(components);
				*/
				if (components && 
					components->list_start,
					types->list_start->name,
					components->list_end->name)
				{
					if (!strcmp(types->list_start->name,
						components->list_end->name))
					{
						if (i->constructor == constructor)
							return rmldb_get_type_comp_from_con(rmldb_get_type_components(i), index);
						/*
						if (i->is_transparent)
						{
							return rmldb_get_type_component(rmldb_get_type_components(i), 0);
						}
						*/
					}
				}
				rmldb_str_list_free(components);
			}
		}
		rmldb_str_list_free(types);
	}
	return NULL;
}


int rmldb_is_type_transparent(rmldb_type_t *type)
{
	rmldb_con_db_t *i;
	if (type)
	{
		rmldb_str_list_t* types;
		types = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
		types->list_start = types->list_end = NULL;
		rmldb_type_to_list(type, types);
		/*
		rmldb_print_type(type, 0, stderr);
		rmldb_str_list_print(types);
		*/
		if (types && types->list_start, types->list_end->name)
		{
			for(i = rmldb_con_db_start; i; i = i->next)
			{
				rmldb_str_list_t* components;
				components = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
				components->list_start = components->list_end = NULL;
				rmldb_type_to_list(i->type_db->type, components);
				/*
				rmldb_print_type(i->type_db->type, 0, stderr);
				rmldb_str_list_print(components);
				*/
				if (components && 
					components->list_start,
					types->list_start->name,
					components->list_end->name)
				{
					if (!strcmp(types->list_start->name,
						components->list_end->name))
					{
						if (i->is_transparent)
						{
							return 1;
						}
					}
				}
				rmldb_str_list_free(components);
			}
		}
		rmldb_str_list_free(types);
	}
	return 0;
}

char* rmldb_str_list_get_index(rmldb_str_list_t* list, int index)
{
	rmldb_str_t* tmp;
	for (tmp = list->list_start; tmp; tmp = tmp->next)
		if(tmp->depth == index) return tmp->name;
	return NULL;
}

void rmldb_str_list_free(rmldb_str_list_t* list)
{
	rmldb_str_t* tmp, *temp;
	tmp = list->list_start;
	while (tmp)
	{
		temp = tmp;
		tmp = tmp->next;
		free(temp);
	}
	free(list);
}

void rmldb_print_increment(int depth)
{
	int d = 0;
	while(d < depth) { printf(" "); d++; }
}

rmldb_type_t* rmldb_type_unfold(rmldb_type_t* type)
{
    if (type)
	   return type->component.c->list->list_start;
    else 
        return NULL;
}

void rmldb_var_show(void *p, rmldb_type_t* type, int depth)
{
	int remeber = 0;
	/* un-comment if you need debugging */
	/*
	if (type) 
	{ 
		fprintf(stdout,"\nTYPE="); 
		rmldb_print_type(type, 0, stdout); 
		fprintf(stdout,"\n");
	}
	else
	{
		fprintf(stdout,"\nTYPE=NULL");
		fprintf(stdout,"\n");
	}
	*/
	/* if we reached the depth, (if is not 0 = max) exit */
	if (rmldb_depth_of_variable_print && depth > rmldb_depth_of_variable_print) return; 
	if (type && type->kind == RMLDB_eVARty) type = NULL;
	/* oh, shit, another problem, TRANSPARENT SHIT, we should forbid this stuff */
	if (rmldb_is_type_transparent(type))
	{
		rml_uint_t ctor = 0;
		struct rml_struct *po = (void*)malloc(2*sizeof(void*));
		po->header = RML_STRUCTHDR(1, ctor);
		po->data[0] = p;
		p = RML_TAGPTR(po);
		/* fprintf(stdout,"TYPE IS TRANSPARENT\n"); fprintf(stdout,"\n"); */
		/* damn, I need to think at a way to free this indirection */
		remeber = 1;
	}
	rmldb_print_increment(depth);

	if( RML_ISIMM(p) ) 
	{
		if (type)
		{
			if(type->kind==RMLDB_eCONSty && !strcmp(type->component.c->id, "bool"))
			{
				if (RML_UNTAGFIXNUM(p))
					printf ("true:%s", type->component.c->id);
				else
					printf ("false:%s", type->component.c->id);
			}
			else if(type->kind==RMLDB_eCONSty && strcmp(type->component.c->id, "int"))
			{
				/* enumtype??? */
				char *con_name = NULL;
				con_name = rmldb_get_con_name(type, RML_UNTAGFIXNUM(p));
				if (con_name) 
					printf ("%d:enum:%s", RML_UNTAGFIXNUM(p),con_name);
				else printf ("%d:int", RML_UNTAGFIXNUM(p));
			}
			else
				if(type->kind==RMLDB_eRELty)
				{
					printf ("%p:function (same module)", p);
					fprintf(stdout,":"); 
					rmldb_print_type(type, 0, stdout); 
					fprintf(stdout,"\n");
				}
				else
					printf ("%d:int", RML_UNTAGFIXNUM(p));
		}
		else printf ("%d:int", RML_UNTAGFIXNUM(p));
	} 
	else 
	{
		rml_uint_t phdr = RML_GETHDR(p);            
		if( phdr == RML_REALHDR ) 
		{
			printf ("%f:real", rml_prim_get_real(p));
			fflush(stdout);
		} else 
			if( RML_HDRISSTRING(phdr) ) 
			{
				if (rmldb_max_string_variable_print)
				{
					if (rmldb_max_string_variable_print >= RML_HDRSTRLEN(phdr))
						printf ("\"%s\":string", RML_STRINGDATA(p)); 
					else
					{
						char format[100];
						snprintf(format, 100, "\"%c%c%d%c\"...:string", '%', '.', rmldb_max_string_variable_print, 's');
						printf(format, RML_STRINGDATA(p));
					}
				}
				else printf ("\"%s\":string", RML_STRINGDATA(p));
				fflush(stdout);
					/* use if neccesarry RML_HDRSTRLEN(phdr) */
			} 
			else 
			{
				if( RML_HDRISSTRUCT(phdr) ) 
				{
					rmldb_str_list_t* components;
					char* con_name;
					rml_uint_t slots = RML_HDRSLOTS(phdr);
					rml_uint_t constr = RML_HDRCTOR(phdr);
					int suffix = RMLDB_TYPE_SUFFIX_NONE; /* 1-lists, 2-option, 3-lvar */
					if(type && type->kind==RMLDB_eRELty) /* deal with function pointers */
					{
						printf ("%p:function (external module)", p);
						fprintf(stdout,":"); 
						rmldb_print_type(type, 0, stdout); 
						fprintf(stdout,"\n");
						return;
					}
					if (type && type->kind == RMLDB_eTUPLEty)
					{
						int i;
						printf ("(");
						void **pp = RML_STRUCTDATA(p);
						for (i = 0; i < slots; i++)
						{
							/* tree like display please */
							printf ("\n");
							rmldb_var_show(*pp++, 
								rmldb_get_type_comp_from_con(type->component.t->list, i), 
								depth+1);
							if (i < slots-1) printf (" * ");
							fflush(stdout);
						}
						printf (")"); fflush(stdout);
						return;
					}
					if (type && type->kind == RMLDB_eLISTty)
					{
						int i;
						printf ("(");
						void **pp = RML_STRUCTDATA(p);
						for (i = 0; i < slots; i++)
						{
							/* tree like display please */
							printf ("\n");
							rmldb_var_show(*pp++, 
								rmldb_get_type_comp_from_con(type->component.l, i), 
								depth+1);
							if (i < slots-1) printf (" * ");
							fflush(stdout);
						}
						printf (")"); fflush(stdout);
						return;
					}
					if (type)
					{
						components = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
						components->list_start = components->list_end = NULL;
						rmldb_type_to_list(type, components);
						/*
						rmldb_print_type(type, 0, stderr);
						rmldb_str_list_print(components);
						*/
						/*
						RML_STRUCTHDR(slots,ctor);
						#define RML_NILHDR		RML_STRUCTHDR(0,0)
						#define RML_CONSHDR		RML_STRUCTHDR(2,1)
						*/
						if (components->list_end->name)
						{
							if (!strcmp(components->list_end->name, "list"))   
							{
								suffix = RMLDB_TYPE_SUFFIX_LIST;
							}
							if (!strcmp(components->list_end->name, "option")) 
							{
								suffix = RMLDB_TYPE_SUFFIX_OPTION;
							}
							if (!strcmp(components->list_end->name, "vector")) 
							{
								suffix = RMLDB_TYPE_SUFFIX_VECTOR;
							}
							if (!strcmp(components->list_end->name, "array")) 
							{
								suffix = RMLDB_TYPE_SUFFIX_ARRAY;
							}
							if (!strcmp(components->list_end->name, "lvar"))   
							{
								suffix = RMLDB_TYPE_SUFFIX_LVAR;
							}
						}
					}
					/* printf ("S(%d)[%d]=", constr, slots); */
					if (suffix == RMLDB_TYPE_SUFFIX_NONE)
					{
						con_name = rmldb_get_con_name(type, constr);
						if (con_name) 
						{
							if (slots == 0)
								printf ("%s[%d]", con_name, slots);
							else
								printf ("%s[%d](", con_name, slots);
						}
					}
					else 
					{
						if ((suffix != RMLDB_TYPE_SUFFIX_VECTOR) &&
							(suffix != RMLDB_TYPE_SUFFIX_ARRAY))
						{
							if (slots == 0 && constr == 0) 
							{
								printf ("NIL");
							}
							else
							/* if we haven't found the CONSTRUCTOR NAME print it as it is */
							if (slots == 2 && constr == 1) 
							{ 
								/* printf("RML.cons("); */
								suffix = RMLDB_TYPE_SUFFIX_LIST;
							}
							else 
								if (slots == 1 && constr == 1) 
								{ 
									/* printf("RML.SOME("); */
									suffix = RMLDB_TYPE_SUFFIX_OPTION;
								}
							else if (!remeber)/* fall to the most miserable case */ 
									printf ("S(%d)[%d](", constr, slots);
						}
					}
					void **pp = RML_STRUCTDATA(p);
					fflush(stdout);
					/* function definition  - check this here by the variable type
					   if is a function then is RELty as type !!!!! */
					if ((constr == 64 || constr==13) &&
						slots > 1000000) return;
					if( slots != 0 )
					{
						int i = 0; 
						/* treat here vectors separately */
						if (suffix == RMLDB_TYPE_SUFFIX_VECTOR)
						{
							printf ("VECTOR[%d]{", slots);
							for (i = 0; i < slots; i++)
							{
								/* tree like display please */
								printf ("{\n");
								rmldb_var_show(*pp++, 
									rmldb_type_unfold(type), 
									depth+1);
								if (i < slots-1) printf ("},");
								else printf ("}");
								fflush(stdout);
							}
							printf ("}");
							fflush(stdout);
							return;
						}
						/* treat here arrays separately */
						if (suffix == RMLDB_TYPE_SUFFIX_ARRAY)
						{
							printf ("ARRAY[%d]{", slots);
							for (i = 0; i < slots; i++)
							{
								/* tree like display please */
								printf ("{\n");
								rmldb_var_show(*pp++, 
									rmldb_type_unfold(type), 
									depth+1);
								if (i < slots-1) printf ("},");
								else printf ("}");
								fflush(stdout);
							}
							printf ("}");
							fflush(stdout);
							return;
						}
						/* treat here lists separately */
						if (suffix == RMLDB_TYPE_SUFFIX_LIST)
						{
							/*
							#define RML_NILHDR		RML_STRUCTHDR(0,0)
							#define RML_CONSHDR		RML_STRUCTHDR(2,1)
							*/
							printf ("[");
							while (phdr == RML_CONSHDR)
							{
								void *pcar = RML_CAR(p);
								void *pcdr = RML_CDR(p);
								/* list = RML.cons(car, cdr) | RML.nil */
								/* cdr */
								phdr = RML_GETHDR(pcdr);
								printf ("\n");
								/* print car */
								rmldb_var_show(pcar, 
									rmldb_type_unfold(type), 
									depth+1);
								if (phdr == RML_CONSHDR) 
								{
									printf (",");
									/* print cdr 
									rmldb_var_show(*pcdr, 
										type, 
										depth+1);
									*/
									p = pcdr;
								}
							}
							printf ("]");
							fflush(stdout);
							return;
						}
						/* treat option separate also */
						if (suffix == RMLDB_TYPE_SUFFIX_OPTION)
						{
							if (phdr == RML_NILHDR) /* NONE */
							{
								printf ("NONE");
							}
							else
							{
								/* phdr = RML_STRUCTHDR(1,1) = SOME(x) */
								void **pcar = RML_STRUCTDATA(p);
								printf ("SOME(\n");
								/* pcar++; */
								rmldb_var_show(*pcar, 
									rmldb_type_unfold(type), 
									depth+1);
								printf (")");
							}
							fflush(stdout);
							return;
						}
						/* treat option separate also */
						if (suffix == RMLDB_TYPE_SUFFIX_LVAR)
						{
							/* actually should not happen here */
							printf ("LVAR?!?!?");
							return;
						}
						/* the last case DATATYPES */
						for (i = 0; i < slots; i++)
						{
							/* tree like display please */
							printf ("\n");
							rmldb_var_show(*pp++, 
								rmldb_get_type_component(type, constr, i), 
								depth+1);
							if (i < slots-1) printf (",");
							fflush(stdout);
						}
						if (remeber) 
						{
							/* we can free the p now here */
							free (RML_UNTAGPTR(p));
							remeber = 0;
						}
						printf (")"); fflush(stdout);
					}					    
				} 
				else 
				{
					printf ("LVAR(UNKNOWN)"); fflush(stdout);
				}
			}
	}
}

char* rmldb_get_named_type(rmldb_type_t* type, char* name1, char* name2)
{
    if (!type) return "[unknown]";
    if (type->name == NULL)
      if (name1) return name1; else return name2; 
    else
      return type->name;
}

void rmldb_var_send(void *p, rmldb_type_t* type, int depth, int structDepth)
{
	/*
	java expects this!
	depth|ty|vName|vValue|vType\n
	where ty:
	  st<ring>,
	  da<tatype constructor>,
	  tu<ple>,
	  li<st>,
	  en<enum>,
	  op<option>,
	  lv<ar>,
	  re<al>,
	  in<t>,
	  bo<olean>
	  fu<nction>
	  ve<ctor>
      ar<ray>
	*/
	char line[RMLDB_MAX_LINE];
    char* componentName;
	int remeber = 0;
	/* if we reached the depth, (if is not 0 = max) exit */
	if (rmldb_depth_of_variable_print && depth > rmldb_depth_of_variable_print) return;
    /* build the component name */
    if (structDepth < 0) 
        componentName = NULL;  
    else 
    {
        componentName = (char*)malloc(21*sizeof(char)); 
        snprintf(componentName, 20, "[%d]", structDepth);
    }
	if (type && type->kind == RMLDB_eVARty) type = NULL;
	/* oh, shit, another problem, TRANSPARENT SHIT, we should forbid this stuff */
	/* debug if (type ) rmldb_print_type(type, 1, stderr); */
	if (rmldb_is_type_transparent(type))
	{
		unsigned ctor = 0;
		struct rml_struct *po = (void*)malloc(2*sizeof(void*));
		po->header = RML_STRUCTHDR(1, ctor);
		po->data[0] = p;
		p = RML_TAGPTR(po);
		/* fprintf(stdout,"TYPE IS TRANSPARENT\n"); fprintf(stdout,"\n"); */
		/* damn, I need to think at a way to free this indirection */
		remeber = 1;
	}
	if( RML_ISIMM(p) ) 
	{
		if (type)
		{
			if(type->kind==RMLDB_eCONSty && !strcmp(type->component.c->id, "bool"))
			{
				snprintf(line, RMLDB_MAX_LINE, "%d|bo|%s|%s|%s", depth, 
                    rmldb_get_named_type(type, componentName, "->"), 
                    RML_UNTAGFIXNUM(p)?"true":"false", 
                    type->component.c->id);
				_rmldb_socket_outln(line);
			}
			else if(type->kind==RMLDB_eCONSty && strcmp(type->component.c->id, "int") != 0)
			{
				/* enumtype??? */
				rmldb_con_db_t *con = NULL;
				con = rmldb_get_con(type, RML_UNTAGFIXNUM(p));
				if (con)
				{
					snprintf(line, RMLDB_MAX_LINE, "%d|en|%s|%d:enum:%s|", 
                        depth, 
                        rmldb_get_named_type(type, componentName, "->"), 
                        RML_UNTAGFIXNUM(p), 
                        con->name);
					_rmldb_socket_out(line);
					rmldb_type_to_sock(con->type_db->type, 0);
					_rmldb_socket_outln("");
				}
				else 
				{
					snprintf(line, RMLDB_MAX_LINE, "%d|in|%s|%d|int", 
                        depth, 
                        rmldb_get_named_type(type, componentName, "->"), 
                        RML_UNTAGFIXNUM(p));
					_rmldb_socket_outln(line);
				}
			}
			else
            {
				if(type->kind==RMLDB_eRELty)
				{
					snprintf(line, RMLDB_MAX_LINE, "%d|fu|%s|%p:function (same module)|", 
                        depth, 
                        rmldb_get_named_type(type, componentName, "->"), 
                        p); 
					_rmldb_socket_out(line);
					rmldb_type_to_sock(type, 0);
					_rmldb_socket_outln("");
				}
				else
				{
					snprintf(line, RMLDB_MAX_LINE, "%d|in|%s|%d|int", 
                        depth, 
                        rmldb_get_named_type(type, componentName, "->"), 
                        RML_UNTAGFIXNUM(p));
					_rmldb_socket_outln(line);
				}
            }
		}
		else 
		{
			snprintf(line, RMLDB_MAX_LINE, "%d|in|%s|%d|int", 
                depth, 
                rmldb_get_named_type(type, componentName, "->"), 
                RML_UNTAGFIXNUM(p));
			_rmldb_socket_outln(line);
		}
	} 
	else 
	{
		rml_uint_t phdr = RML_GETHDR(p);            
		if( phdr == RML_REALHDR ) 
		{
			snprintf(line, RMLDB_MAX_LINE, "%d|re|%s|%f|real", 
                depth, 
                rmldb_get_named_type(type, componentName, "->"), 
                rml_prim_get_real(p));
			_rmldb_socket_outln(line);
		} else 
			if( RML_HDRISSTRING(phdr) ) 
			{
				int idx = 0;
				snprintf(line, RMLDB_MAX_LINE, "%d|st|%s|-|string", 
                    depth, 
                    rmldb_get_named_type(type, componentName, "->"));
				_rmldb_socket_outln(line);
				/* send all the string data */
				_rmldb_socket_outln("<.$STRING_START$.>");
				if (RML_HDRSTRLEN(phdr) <= RMLDB_MAX_LINE)
				{
					_rmldb_socket_outln(RML_STRINGDATA(p));
				}
				else
				{
					for (idx = 0; idx < RML_HDRSTRLEN(phdr); idx++)
					{
						sprintf(line, "%c", RML_STRINGDATA(p)[idx]); 
						_rmldb_socket_outln(line);
					}
				}
				_rmldb_socket_outln("<.$STRING_END$.>");
			} 
			else 
			{
				if( RML_HDRISSTRUCT(phdr) ) 
				{
					int i = 0; 
					rmldb_str_list_t* components;
					char* con_name;
					rml_uint_t slots = RML_HDRSLOTS(phdr);
					rml_uint_t constr = RML_HDRCTOR(phdr);
					int suffix = RMLDB_TYPE_SUFFIX_NONE; /* 1-lists, 2-option, 3-lvar */
					if(type && type->kind==RMLDB_eRELty) /* deal with function pointers */
					{
						snprintf(line, RMLDB_MAX_LINE, "%d|fu|%s|%p:function (external module)|", 
                            depth, 
                            rmldb_get_named_type(type, componentName, "->"), 
                            p);
						_rmldb_socket_out(line);
						rmldb_type_to_sock(type, 0);
						_rmldb_socket_outln("");
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                        
						return;
					}
					if (type && (type->kind == RMLDB_eTUPLEty || type->kind == RMLDB_eLISTty))
					{
						int i;
						snprintf(line, RMLDB_MAX_LINE, "%d|tu|%s|TUPLE[%d](%d)|", 
                            depth, 
                            rmldb_get_named_type(type, componentName, "[tuple]"), 
                            slots, 
                            type->kind);
						_rmldb_socket_out(line);
						rmldb_type_to_sock(type, 0);
						_rmldb_socket_outln("");
						void **pp = RML_STRUCTDATA(p);
						for (i = 0; i < slots; i++)
						{
							if (type->kind == RMLDB_eTUPLEty)
							{
							  rmldb_var_send(*pp++, 
								rmldb_get_type_comp_from_con(type->component.t->list, i), 
								depth+1, i);
							}
							if (type->kind == RMLDB_eLISTty)
							{
							  rmldb_var_send(*pp++, 
								rmldb_get_type_comp_from_con(type->component.l, i), 
								depth+1, i);
							}							
						}
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                                                
						return;
					}
					if (type)
					{
						components = (rmldb_str_list_t*)malloc(sizeof(rmldb_str_list_t));
						components->list_start = components->list_end = NULL;
						rmldb_type_to_list(type, components);
						/*
						rmldb_print_type(type, 0, stderr);
						rmldb_str_list_print(components);
						*/
						/*
						RML_STRUCTHDR(slots,ctor);
						#define RML_NILHDR		RML_STRUCTHDR(0,0)
						#define RML_CONSHDR		RML_STRUCTHDR(2,1)
						*/
						if (components->list_end->name)
						{
							if (!strcmp(components->list_end->name, "list"))   
							{
								suffix = RMLDB_TYPE_SUFFIX_LIST;
							}
							if (!strcmp(components->list_end->name, "option")) 
							{
								suffix = RMLDB_TYPE_SUFFIX_OPTION;
							}
							if (!strcmp(components->list_end->name, "vector")) 
							{
								suffix = RMLDB_TYPE_SUFFIX_VECTOR;
							}
							if (!strcmp(components->list_end->name, "array")) 
							{
								suffix = RMLDB_TYPE_SUFFIX_ARRAY;
							}
							if (!strcmp(components->list_end->name, "lvar"))   
							{
								suffix = RMLDB_TYPE_SUFFIX_LVAR;
							}
						}
					}
					void **pp = RML_STRUCTDATA(p);
					/* function definition  - check this here by the variable type
					   if is a function then is RELty as type !!!!! */
					if ((constr == 64 || constr==13) && slots > 1000000) 
                    {
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                        
                        return;
                    }
					/* treat here vectors separately */
					if (suffix == RMLDB_TYPE_SUFFIX_VECTOR)
					{
						snprintf(line, RMLDB_MAX_LINE, "%d|ve|%s|VECTOR[%d]|", 
                            depth, 
                            rmldb_get_named_type(type, componentName, "[vector]"), 
                            slots);
						_rmldb_socket_out(line);
						rmldb_type_to_sock(type, 0);
						_rmldb_socket_outln("");
						for (i = 0; i < slots; i++)
						{
							rmldb_var_send(*pp++, rmldb_type_unfold(type), depth+1, i);
						}
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                                                
						return;
					}
					/* treat here arrays separately */
					if (suffix == RMLDB_TYPE_SUFFIX_ARRAY)
					{
						snprintf(line, RMLDB_MAX_LINE, "%d|ar|%s|ARRAY[%d]|", 
                            depth, 
                            rmldb_get_named_type(type, componentName, "[array]"), 
                            slots);
						_rmldb_socket_out(line);
						rmldb_type_to_sock(type, 0);
						_rmldb_socket_outln("");
						for (i = 0; i < slots; i++)
						{
							rmldb_var_send(*pp++, rmldb_type_unfold(type), depth+1, i);
						}
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                                                
						return;
					}
					/* treat here lists separately */
					if (suffix == RMLDB_TYPE_SUFFIX_LIST)
					{
						/*
						#define RML_NILHDR		RML_STRUCTHDR(0,0)
						#define RML_CONSHDR		RML_STRUCTHDR(2,1)
						*/
                        int listDepth = 0;
						if (phdr == RML_CONSHDR) 
							snprintf(line, RMLDB_MAX_LINE, "%d|li|%s|LIST|", 
                                depth, 
                                rmldb_get_named_type(type, componentName, "[list]"));
						else /* nil header */
							snprintf(line, RMLDB_MAX_LINE, "%d|li|%s|NIL|", 
                                depth, 
                                rmldb_get_named_type(type, componentName, "[list]"));
						_rmldb_socket_out(line);
						rmldb_type_to_sock(type, 0);
						_rmldb_socket_outln("");
						while (phdr == RML_CONSHDR)
						{
							void *pcar = RML_CAR(p);
							void *pcdr = RML_CDR(p);
							/* list = RML.cons(car, cdr) | RML.nil */
							/* cdr */
							phdr = RML_GETHDR(pcdr);
							/* print car */
							rmldb_var_send(pcar, 
								rmldb_type_unfold(type), 
								depth+1, listDepth);
                            listDepth++;
							if (phdr == RML_CONSHDR) 
							{
								p = pcdr;
							}
							else /* nil hdr */
							{
								/* do not send nil, is no need
								snprintf(line, RMLDB_MAX_LINE, "%d|li|LIST_END|", depth);
								_rmldb_socket_out(line);
								rmldb_type_to_sock(type, 0);
								_rmldb_socket_outln("");
								*/
							}
						}
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                                                
						return;
					}
					/* treat option separate also */
					if (suffix == RMLDB_TYPE_SUFFIX_OPTION)
					{
						if (phdr == RML_NILHDR) /* NONE */
						{
							snprintf(line, RMLDB_MAX_LINE, "%d|op|%s|NONE[0]|", 
                                depth, 
                                rmldb_get_named_type(type, componentName, "[option]"));
							_rmldb_socket_out(line);
							rmldb_type_to_sock(type, 0);
							_rmldb_socket_outln("");							
						}
						else
						{
							/* phdr = RML_STRUCTHDR(1,1) = SOME(x) */
							void **pcar = RML_STRUCTDATA(p);
							snprintf(line, RMLDB_MAX_LINE, "%d|op|%s|SOME[1]|", 
                                depth, 
                                rmldb_get_named_type(type, componentName, "[option]"));
							_rmldb_socket_out(line);
							rmldb_type_to_sock(type, 0);
							_rmldb_socket_outln("");							
							/* pcar++; */
							rmldb_var_send(*pcar, 
								rmldb_type_unfold(type), 
								depth+1, -1);
						}
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                                                
						return;
					}
					/* treat lvar separate also */
					if (suffix == RMLDB_TYPE_SUFFIX_LVAR)
					{
						/* actually should not happen here 
						* yes, it should, fold the type
						*/
						snprintf(line, RMLDB_MAX_LINE, "%d|lv|%s|LVAR[1]|", 
                            depth, 
                            rmldb_get_named_type(type, componentName, "[lvar]"));
						_rmldb_socket_out(line);
						rmldb_type_to_sock(type, 0);
						_rmldb_socket_outln("");
                        /* free our stuff */
                        if (componentName) { free(componentName); componentName = NULL; }
                        if (remeber) 
                        {
                            /* we can free the p now here */
                            free (RML_UNTAGPTR(p));
                            remeber = 0;
                        }                                                			
						return;
					}
					
					/* the last case DATATYPES */
					/* print the constructor here */
					if (type) 
					{
						rmldb_con_db_t *con = NULL;
						con = rmldb_get_con(type, constr);
						if (con)
						{
							snprintf(line, RMLDB_MAX_LINE, "%d|da|%s|%s[%d]|", 
                                depth,
                                rmldb_get_named_type(type, componentName, "[record]"), 
                                con->name, 
                                slots);
							_rmldb_socket_out(line);
							rmldb_type_to_sock(con->type_db->type, 0);
							_rmldb_socket_outln("");
						}
						else
						{
							snprintf(line, RMLDB_MAX_LINE, "%d|da|%s|UNKNOWN-STRUCTURE[%d,%d]|", 
                                depth, 
                                rmldb_get_named_type(type, componentName, "[structure]"), 
                                constr, 
                                slots);
							_rmldb_socket_out(line);
							rmldb_type_to_sock(type, 0);
							_rmldb_socket_outln("");
						}
					}
					else
					{
							snprintf(line, RMLDB_MAX_LINE, "%d|da|%s|UNKNOWN-STRUCTURE[%d,%d]|", 
                                depth,
                                rmldb_get_named_type(type, componentName, "[structure]"),
                                constr, 
                                slots);
							_rmldb_socket_out(line);
							rmldb_type_to_sock(type, 0);
							_rmldb_socket_outln("");
					}
					/* print the constructor components here */
					for (i = 0; i < slots; i++)
					{
						/* tree like display please */
                        /* fprintf(stderr, "printing slot %d\n", i); */
						rmldb_var_send(*pp++, rmldb_get_type_component(type, constr, i), depth+1, i);
                        /* fprintf(stderr, "done printing slot %d\n", i); */                        
					}
				} 
				else 
				{
					snprintf(line, RMLDB_MAX_LINE, "%d|lv|%s|LVAR(NOT_BOUND)|", 
                        depth, 
                        rmldb_get_named_type(type, componentName, "[lvar]")); 
					_rmldb_socket_out(line);
					rmldb_type_to_sock(type, 0);
					_rmldb_socket_outln("");
				}
			}
	}
    /* free our stuff */
    if (componentName) { free(componentName); componentName = NULL; }
    if (remeber) 
    {
        /* we can free the p now here */
        free (RML_UNTAGPTR(p));
        remeber = 0;
    }                        
    
}

char* rmldb_directionString(int direction)
{
	switch(direction)
	{
		case RMLDB_VAR_IN: return "input";
		case RMLDB_VAR_OUT: return "output";
		default: return "unknown direction";	
	}
}

int rmldb_print_active_var(char* var_name)
{
	int printed = 0;
	int i = 0;
	rmldb_type_db_t* type_db;
	/* 
   * fprintf(stderr, "NOTE that the depth of printing is set to: %d\n", rmldb_depth_of_variable_print);
   */
	for(i = 0; i < rmldb_number_of_vars; i ++)
		if (strcmp(rmldb_vars[i]->var_name, var_name) == 0)
		{
			printed = 1;
			type_db = rmldb_get_type_db(var_name);
			if (type_db)
			{
				fprintf(stderr, "\nVARIABLE %s IS [%s] and HAS TYPE: ", var_name, rmldb_directionString(rmldb_vars[i]->direction));
				rmldb_print_type(type_db->type, 0, stdout);
				fprintf(stderr, "\n");
				if (type_db->type->kind == RMLDB_eVARty)
				{
					rmldb_var_print(rmldb_vars[i]->var);
				}
				else
				{
					fprintf(stderr, " %s=", var_name);
					rmldb_var_show(rmldb_vars[i]->var, type_db->type, 0);
				}
			}
			else
			{
				fprintf(stderr, "\nI couldn't find the type of variable %s! printing without types \n", var_name);
				fprintf(stderr, "[%s] %s=", rmldb_directionString(rmldb_vars[i]->direction), var_name);
				rmldb_var_print(rmldb_vars[i]->var);
			}
		}
	if (!printed) fprintf(stderr, "[not in current context]");
	fprintf(stderr,"\n");
	fflush(stderr);
	return RMLDB_SUCCESS;
}


int rmldb_print_active_vars(void)
{
	int i = 0;
	rmldb_type_db_t* type_db;
	for(i = 0; i < rmldb_number_of_vars; i ++)
	{
		fprintf(stderr, "%s ", rmldb_vars[i]->var_name);
		rmldb_print_active_var(rmldb_vars[i]->var_name);
	}
	fprintf(stderr,"\n");
	fflush(stderr);
	return RMLDB_SUCCESS;
}


#endif /* RML_DEBUG */

/* ******************************************************************************************** */

/* debug-print */
RML_BEGIN_LABEL(RML__debug_5fprint)
{
	void *str = rmlA0;
	printf ("%s=[", RML_STRINGDATA(str));
	rmldb_var_print(rmlA1);
	printf ("]\n");
	fflush(stdout);
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


int rmldb_save_stack_pointer = -1;
int rmldb_stack_pointer_saved = 0;
int rmldb_suspend_type = 0;

/* debug-debug.c */
RML_BEGIN_LABEL(RML__debug)
{
#ifdef RML_DEBUG
	
	RML__call_debug(
		RML_STRINGDATA(rmlA0),   /* file */
		RML_UNTAGFIXNUM(rmlA1),  /* start position */
		RML_UNTAGFIXNUM(rmlA2),  /* end position */		 
		RML_UNTAGFIXNUM(rmlA3),  /* start line */
		RML_UNTAGFIXNUM(rmlA4),  /* start column */
		RML_UNTAGFIXNUM(rmlA5),  /* end line */
		RML_UNTAGFIXNUM(rmlA6),  /* end column */
		RML_STRINGDATA(rmlA7),   /* function */
		RML_STRINGDATA(rmlA8));  /* call */

#endif /* RML_DEBUG */

	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/*
 * A0=fileName:string
 * A1=sline:int, A2=scolumn:int, A3=eline:int, A4=ecolumn:int
 * A5=relation:string
 * A6=goal:string
 */
void RML__call_debug(char* fileName, int sp, int ep, int sl, int sc, int el, int ec, char* relation, char* call)
{
#ifdef RML_DEBUG
	if (rml_debug_enabled)
	{
	    int i;
	    char *str = fileName;
	    char *strrel = relation; void *strgoal = call; 
	    char s[RMLDB_MAX_STRING] = ""; 
	    char buf[RMLDB_MAX_STRING] = "";     
	    snprintf(s, RMLDB_MAX_STRING, "%s:%d.%d.%d.%d within[%s] -> [%s]", 
					str,		/* file */ 
					sl,		/* start line */
					sc,		/* start column */
					el,		/* end line */
					ec,     /* end column */
					strrel,     /* relation */
					strgoal     /* goal (term) */
					);	
		rmldb_current_execution_loc.file = fileName;
		rmldb_current_execution_loc.relation = relation;
		rmldb_current_execution_loc.goal = call;
		rmldb_current_execution_loc.range.sl = sl;
		rmldb_current_execution_loc.range.sc = sc;
		rmldb_current_execution_loc.range.el = el;
		rmldb_current_execution_loc.range.ec = ec;

		if (rmldb_execution_type == RMLDB_TRACE_ALL)
		{
      printf("%s\n", s);
      rmldb_print_active_vars();
			rmldb_clear_active_vars();
	  	return;
		}

		if (rmldb_execution_type == RMLDB_STEP_RETURN) /* do we jump over the goal? */
		{
			/* TODO! deal with this later! */
			rmldb_execution_type = RMLDB_STEP;
			rmldb_suspend_type = RMLDB_STEP_RETURN;	
		}	
	
		if (rmldb_execution_type == RMLDB_STEP_OVER) /* do we jump over the goal? */
		{
			/* save the current stack pointer */
			if (!rmldb_stack_pointer_saved)
			{
				rmldb_save_stack_pointer = rmldb_stack_point; /* as we already pushed one before */
				rmldb_stack_pointer_saved = 1;
			}
			if (rmldb_save_stack_pointer != -1 && rmldb_save_stack_pointer == rmldb_stack_point) /* we came below, to the caller, we did the step */
			{
				rmldb_save_stack_pointer = -1;
				rmldb_stack_pointer_saved = 0;
				rmldb_execution_type = RMLDB_STEP;
				rmldb_suspend_type = RMLDB_STEP_OVER;			
			}
		}
		
		rmldb_push_stack_frame(fileName, sp, ep, sl, sc, el, ec, relation, call);
		/* clear the pushed vars */
		rmldb_clear_active_vars();
	
		/* check for breakpoints */
	    if (rmldb_execution_type != RMLDB_STEP && rmldb_number_of_breakpoints != 0)
	    {
	        for (i = 0; i < rmldb_number_of_breakpoints; i++)
	        {
	        	if (rmldb_breakpoints[i]->ty == RMLDB_CMD_BREAKPOINT)
	    			if (strcmp(rmldb_breakpoints[i]->data.brkFile.file, fileName) == 0 &&
	    				rmldb_breakpoints[i]->data.brkFile.line == sl)
		            {
		                rmldb_execution_type = RMLDB_STEP;
		                break;
		            }
	        	if (rmldb_breakpoints[i]->ty == RMLDB_CMD_FUNCTION_BREAKPOINT)
	    			if (strcmp(rmldb_breakpoints[i]->data.brkFunction.file, fileName) == 0 &&
	    				strcmp(rmldb_breakpoints[i]->data.brkFunction.name, relation) == 0)
		            {
		                rmldb_execution_type = RMLDB_STEP;
		                break;
		            }	            
	        } 
	        rmldb_suspend_type = RMLDB_BREAKPOINT;   
	    }
	
		if (rmldb_execution_type == RMLDB_RUN) /* do we jump over the goal? */
		{
			/* TODO! deal with this later! */
			/* do nothing! */
		}
	
	    /* test if we are stepping */
	    if (rmldb_execution_type == RMLDB_STEP) /* do we step? */
	    {
	    	char buffer[2000];
	    	switch(rmldb_suspend_type)
	    	{
	    		case RMLDB_BREAKPOINT:
	        		sprintf(buffer, "%s %s:%d", RMLDB_EVENT_BREAKPOINT, str, sl);
	        		break;
	    		case RMLDB_STEP_OVER:
	        		sprintf(buffer, "%s %s:%d", RMLDB_EVENT_STEP_OVER, str, sl);
	        		break;	        		
	    		case RMLDB_STEP_RETURN:
	        		sprintf(buffer, "%s %s:%d", RMLDB_EVENT_STEP_RETURN, str, sl);
	        		break;	        		
	    		default: /* normal step */
	        		sprintf(buffer, "%s %s:%d", RMLDB_EVENT_STEP, str, sl);
	        		break;	        			        		
	    	}
	    	rmldb_suspend_type = 0;
	        rmldb_sendEvent(buffer);
	
	        /* interpret commands */
	        rmldb_interpretCommand();
	    }
	}
#endif /* RML_DEBUG */
}

/* debug-push-vars.c */
/* all functions depend on 
#ifndef RML_DEBUG   
they will call SC directly and their code will be only in -g lib version 
*/

/* these functions add:
 * rmlAX   variable name
 * rmlAX+1 variable
 * to the formal parameter list of next goal */

RML_BEGIN_LABEL(RML__debug_5fpush_5fin01)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL


RML_BEGIN_LABEL(RML__debug_5fpush_5fin02)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin03)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin04)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin05)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin06)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin07)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin08)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin09)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin10)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA18), rmlA19); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin11)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA20), rmlA21); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin12)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA22), rmlA23); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin13)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA24), rmlA25); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin14)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA24), rmlA25); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA26), rmlA27); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin15)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA24), rmlA25); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA26), rmlA27); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA28), rmlA29); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fin16)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA24), rmlA25); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA26), rmlA27); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA28), rmlA29); 
	rmldb_add_active_var(RMLDB_VAR_IN, RML_STRINGDATA(rmlA30), rmlA31); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

/* these functions add:
 * rmlAX variable name
 * rmlAX+1 variable
 * to the result list of last goal */

RML_BEGIN_LABEL(RML__debug_5fpush_5fout01)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout02)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout03)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout04)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout05)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout06)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout07)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout08)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout09)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout10)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA18), rmlA19); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout11)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA20), rmlA21); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout12)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA22), rmlA23); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout13)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA24), rmlA25); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout14)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA24), rmlA25); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA26), rmlA27); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout15)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA24), rmlA25); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA26), rmlA27); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA28), rmlA29); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fpush_5fout16)
{
#ifndef RML_DEBUG
	RML_TAILCALLK(rmlSC);
#else
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA0), rmlA1); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA2), rmlA3); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA4), rmlA5); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA6), rmlA7); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA8), rmlA9); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA10), rmlA11); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA12), rmlA13); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA14), rmlA15); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA16), rmlA17); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA18), rmlA19); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA20), rmlA21); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA22), rmlA23); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA24), rmlA25); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA26), rmlA27); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA28), rmlA29); 
	rmldb_add_active_var(RMLDB_VAR_OUT, RML_STRINGDATA(rmlA30), rmlA31); 
	RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL

RML_BEGIN_LABEL(RML__debug_5fshow_5fdepth)
{
#ifndef RML_DEBUG
    RML_TAILCALLK(rmlSC);
#else
    rmldb_depth_of_variable_print = RML_UNTAGFIXNUM(rmlA0);
    RML_TAILCALLK(rmlSC);
#endif
}RML_END_LABEL




