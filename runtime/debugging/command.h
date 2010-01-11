#ifndef _RMLDB_COMMAND_H_
#define _RMLDB_COMMAND_H_

/* the commands accepted by the debugger */
#define RMLDB_CMD_NONE 							0000
#define RMLDB_CMD_BREAKPOINT 					1000
#define RMLDB_CMD_DELETE_BREAKPOINT 			1001
#define RMLDB_CMD_CLEAR_BREAKPOINTS 			1002
#define RMLDB_CMD_FUNCTION_BREAKPOINT			1003
#define RMLDB_CMD_DELETE_FUNCTION_BREAKPOINT	1004
#define RMLDB_CMD_STRING_BREAKPOINT 			1005
#define RMLDB_CMD_DELETE_STRING_BREAKPOINT		1006
#define RMLDB_CMD_FAILURE_BREAKPOINT            1007
#define RMLDB_CMD_DELETE_FAILURE_BREAKPOINT     1008
#define RMLDB_CMD_STACK							1009
#define RMLDB_CMD_VARIABLE_VALUE				1010
#define RMLDB_CMD_LAZY_VARIABLE_VALUE			1011
#define RMLDB_CMD_FRAME_VARIABLE_VALUE			1012
#define RMLDB_CMD_FRAME_LAZY_VARIABLE_VALUE		1013

#define RMLDB_CMD_RESUME						2000
#define RMLDB_CMD_STEP							2001
#define RMLDB_CMD_STEP_OVER						2002
#define RMLDB_CMD_STEP_RETURN					2003
#define RMLDB_CMD_RUN							2004
#define RMLDB_CMD_QUIT							2005

/* the response of the debugger to commands */
#define RMLDB_REPLY_OK							"$ok"
#define RMLDB_REPLY_ERROR						"$err"

/* the signals accepted by the debugger  */
#define RMLDB_SIGNAL_SUSPEND					"$u"
#define RMLDB_SIGNAL_QUIT						"$q"


/* the events send by the debugger to the Eclipse UI */
#define RMLDB_EVENT_START						"$t"
#define RMLDB_EVENT_READY						"$rdy"
#define RMLDB_EVENT_STEP						"$s"
#define RMLDB_EVENT_STEP_OVER					"$o"
#define RMLDB_EVENT_STEP_RETURN					"$n"
#define RMLDB_EVENT_RUN							"$r"
#define RMLDB_EVENT_QUIT						"$q"
#define RMLDB_EVENT_SUSPEND						"$u"
#define RMLDB_EVENT_BREAKPOINT					"$b"
#define RMLDB_EVENT_FAILURE_BREAKPOINT          "$bf"
#define RMLDB_EVENT_ERROR						"$err"

typedef struct rmldb_command_t
{
	int ty;   /* the command type */
	union 
	{
		struct brkFile      	 { char *file; int   line;   		} brkFile;
		struct brkFunction  	 { char *file; char *name;   		} brkFunction;		
		struct brkString		 { char *filter;             		} brkString;
        struct brkFailure        { char *filter;                    } brkFailure;        
		struct varValue			 { char *name;       	    		} varValue;
		struct lazyVarValue		 { char *name; int *path;			} lazyVarValue;
		struct frameVarValue     { int frame; int nr;				} frameVarValue;
		struct frameLazyVarValue { int frame; int nr; int *path;	} frameLazyVarValue;
	} data;
} rmldb_command_type;


extern rmldb_command_type* rmldb_current_command;

extern rmldb_command_type* rmldb_mk_cmd(int ty);
extern rmldb_command_type* rmldb_mk_cmd_brkFile          (int ty, char* file, char* line);
extern rmldb_command_type* rmldb_mk_cmd_brkFunction      (int ty, char* file, char* name);
extern rmldb_command_type* rmldb_mk_cmd_brkString        (int ty, char* filter);
extern rmldb_command_type* rmldb_mk_cmd_brkFailure       (int ty, char* filter);
extern rmldb_command_type* rmldb_mk_cmd_varValue         (int ty, char* name);
extern rmldb_command_type* rmldb_mk_cmd_lazyVarValue     (int ty, char* name, char* path);
extern rmldb_command_type* rmldb_mk_cmd_frameVarValue    (int ty, char* frame, char* nr);
extern rmldb_command_type* rmldb_mk_cmd_frameLazyVarValue(int ty, char* frame, char* nr, char* path);
extern int rmldb_freeCommand(rmldb_command_type* cmd);

#endif /* _RMLDB_COMMAND_H_*/
