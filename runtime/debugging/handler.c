#ifdef RML_DEBUG
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include "rml.h"

int aadebug = 0;
int rmldb_last_command = RMLDB_CMD_NONE;

/* adrpo - debugger global variables */
int   rmldb_cmd_port    = -1;  /* listen for commands from the client on this port */
int   rmldb_reply_port  = -1; /* listen on this port and send synchronous reply to commands */
int   rmldb_event_port  = -1; /* listen on this port and send event to the client */
int   rmldb_signal_port = -1; /* listen on this post and wait for async suspend signals from the client */

/* initialize everything to -1 */
rmldb_communication_type rmldb_communicator;

/* this one singals if we are closing the debugging */
int rmldb_is_closing = 0; 

/* What happens if you press CTRL+C  */
void rmldb_use_quit (sig) 
int sig;
{
	fprintf (stderr, "%s CTRL+C encountered. Breaking at next site....\n", RMLDB_PROMPT);
	rmldb_execution_type = RMLDB_STEP;
}

int rmldb_interpretCommand(void)
{
	int parse_status = 0;
	while(1)
	{
		if ((parse_status = aaparse()) != 0)
		{
			rmldb_sendReply(RMLDB_REPLY_ERROR);
		}
		if (rmldb_last_command > RMLDB_CMD_RESUME) break;
	}
	return 0;
}

#if defined(_MSC_VER) || defined(__MINGW32__)
#include <windows.h>
HANDLE rmldb_SignalThreadHandle;
DWORD  rmldb_SignalThreadId;

extern DWORD WINAPI rmldb_SignalListener(void* arg)
#else /* Linux part */
pthread_t rmldb_SignalThreadHandle;
pthread_t rmldb_SignalThreadId;
extern int rmldb_SignalListener(void* arg)
#endif /* Linux */
{
  int size = 0; char event[10];
  while(!rmldb_is_closing) /* repeat until we go down */
  {
    /* we are using the debugger over sockets */
	size = _rmldb_socket_recv(rmldb_communicator.signal_sock, (void*)event, (size_t)10);           
	if ((size == 0) || (size == -1)) 
	{ 
		if (rmldb_socket_debug)		
	      	fprintf (stderr, "%s Error! Receiving from socket failed: %s. Received: %d bytes.\n", 
	      		RMLDB_SOCKET_PROMPT, strerror(_rmldb_socket_errorno), size);
		if (size == -1 ) rmldb_exit (1);
	}   
	else 
	{ 
		event[size] = '\0';
		if (rmldb_socket_debug)
        {		
		    fprintf(stderr, "%s Info! received event :%s.\n", RMLDB_PROMPT, event);
		    fflush(stderr);
        }
		if (!strncmp(event, RMLDB_SIGNAL_SUSPEND, strlen(RMLDB_SIGNAL_SUSPEND)))
			rmldb_execution_type = RMLDB_STEP;
		else if (!strncmp(event, RMLDB_SIGNAL_QUIT, strlen(RMLDB_SIGNAL_QUIT)))
        {
			rmldb_exit(1);
			break;
        }
	}
  }
  return 0;
}

int rmldb_startSignalHandler(void* arg)
{
	/* debug the damn parsers/lexers */
	/* aadebug = 1; */
	/* aarmldbdebug = 1; */
	
/* MINGW32 and Linux - set the SIGINT handler */
#if !defined(_MSC_VER)
	signal (SIGINT, rmldb_use_quit);
#endif	

	/* WSAStartup on Windows! */
	rmldb_socket_initialize_library();
	/* initialize */
	rmldb_communicator.cmd_bind_sock = -1;
	rmldb_communicator.cmd_sock = -1;		
	rmldb_communicator.reply_bind_sock = -1;	
	rmldb_communicator.reply_sock = -1;			
	rmldb_communicator.event_bind_sock = -1;	
	rmldb_communicator.event_sock = -1;			
	rmldb_communicator.signal_bind_sock = -1;	
	rmldb_communicator.signal_sock = -1;			
	
	/* get the sockets */
	rmldb_communicator.cmd_bind_sock    = rmldb_socket_get();
	rmldb_communicator.reply_bind_sock  = rmldb_socket_get();
	rmldb_communicator.event_bind_sock  = rmldb_socket_get();
	rmldb_communicator.signal_bind_sock = rmldb_socket_get();
			
	/* bind and listen */
	rmldb_socket_bind_and_listen(rmldb_communicator.cmd_bind_sock,    "localhost",  rmldb_cmd_port);
	rmldb_socket_bind_and_listen(rmldb_communicator.reply_bind_sock,  "localhost",  rmldb_reply_port);	
	rmldb_socket_bind_and_listen(rmldb_communicator.event_bind_sock,  "localhost",  rmldb_event_port);
	rmldb_socket_bind_and_listen(rmldb_communicator.signal_bind_sock, "localhost",  rmldb_signal_port);	

	/* accept connections */
	rmldb_communicator.cmd_sock     = rmldb_socket_accept(rmldb_communicator.cmd_bind_sock);
	rmldb_communicator.reply_sock   = rmldb_socket_accept(rmldb_communicator.reply_bind_sock);	
	rmldb_communicator.event_sock   = rmldb_socket_accept(rmldb_communicator.event_bind_sock);
	rmldb_communicator.signal_sock  = rmldb_socket_accept(rmldb_communicator.signal_bind_sock);	
	
  /* start the thread that listens for commands */
#if defined(__MINGW32__) || defined(_MSC_VER) /* windows and mingw32 */
  
  if (NULL == (rmldb_SignalThreadHandle = CreateThread(NULL, 0,   rmldb_SignalListener, NULL, 0, &rmldb_SignalThreadId))) {
    fprintf(stderr, "%s failed to start the command listening thread because: %s! Exiting...\n",
    	RMLDB_PROMPT, strerror(GetLastError()));
    rmldb_exit(1);
  }  
#else /* Linux stuff */
  if( pthread_create(&rmldb_SignalThreadId,NULL,&rmldb_SignalListener,NULL)) {
    fprintf(stderr, "%s failed to start the command listening thread because: %s! Exiting...\n",
    	RMLDB_PROMPT, strerror(errno));
    rmldb_exit(1);
  }  
#endif

  /* 
  fflush(stderr);
  fprintf(stdout, "%s Started the debugger. The signal listening thread is online. Sending starting event!\n", RMLDB_PROMPT);
  fflush(stdout);
  */
  
  rmldb_sendEvent(RMLDB_EVENT_START);
  
  /* parse commands here! */
  rmldb_interpretCommand();
  
  return 0;
}

int rmldb_endSignalHandler(void)
{
	/* send the termination event */
	rmldb_sendEvent(RMLDB_EVENT_QUIT);
	/* close the sockets */
	rmldb_socket_close(rmldb_communicator.cmd_sock);
	rmldb_socket_close(rmldb_communicator.cmd_bind_sock);
	rmldb_socket_close(rmldb_communicator.reply_sock);
	rmldb_socket_close(rmldb_communicator.reply_bind_sock);
	rmldb_socket_close(rmldb_communicator.event_sock);
	rmldb_socket_close(rmldb_communicator.event_bind_sock);
	rmldb_socket_close(rmldb_communicator.signal_sock);
	rmldb_socket_close(rmldb_communicator.signal_bind_sock);
	rmldb_is_closing = 1;
	/* clean the signal thread */
#if defined(__MINGW32__) || defined(_MSC_VER) /* windows and mingw32 */
  CloseHandle(rmldb_SignalThreadHandle);
#else
/*
#ifdef HAVE_PTHREAD_YIELD  
    pthread_yield(); // Allowing other thread to shutdown.
#else  
  sched_yield(); // use as backup (in cygwin)
#endif
*/
#endif
	
	/* WSACleanup on Windows */	
	rmldb_socket_cleanup_library();	
}


int rmldb_executeCommand(rmldb_command_type* cmd)
{
	/*
	fprintf(stderr, "Handling command: %d\n", cmd->ty);
	fflush(stderr);
	*/
	rmldb_last_command = cmd->ty;
	switch(cmd->ty)
	{
	case RMLDB_CMD_QUIT:
		rmldb_exit(0);
		break;
	case RMLDB_CMD_BREAKPOINT:
	case RMLDB_CMD_DELETE_BREAKPOINT:
	case RMLDB_CMD_CLEAR_BREAKPOINTS:
	case RMLDB_CMD_FUNCTION_BREAKPOINT:
	case RMLDB_CMD_DELETE_FUNCTION_BREAKPOINT:
	case RMLDB_CMD_STRING_BREAKPOINT:
	case RMLDB_CMD_DELETE_STRING_BREAKPOINT:
    case RMLDB_CMD_FAILURE_BREAKPOINT:
    case RMLDB_CMD_DELETE_FAILURE_BREAKPOINT:    
		if (rmldb_handle_breakpoint_request(cmd) == RMLDB_SUCCESS)
			rmldb_sendReply(RMLDB_REPLY_OK);
		else
			rmldb_sendReply(RMLDB_REPLY_ERROR);
		break;
	case RMLDB_CMD_STACK:
		rmdlb_handle_stack_request(cmd);
		break;
	case RMLDB_CMD_VARIABLE_VALUE:
	case RMLDB_CMD_LAZY_VARIABLE_VALUE:
	case RMLDB_CMD_FRAME_VARIABLE_VALUE:
	case RMLDB_CMD_FRAME_LAZY_VARIABLE_VALUE:
		rmdlb_handle_variable_request(cmd);	
		break;	
	case RMLDB_CMD_STEP:
		rmldb_execution_type = RMLDB_STEP;		
		rmldb_sendReply(RMLDB_REPLY_OK);
		break;
	case RMLDB_CMD_STEP_OVER:
		rmldb_execution_type = RMLDB_STEP_OVER;
		rmldb_sendReply(RMLDB_REPLY_OK);
		break;
	case RMLDB_CMD_STEP_RETURN:
		rmldb_execution_type = RMLDB_STEP_RETURN;	
		rmldb_sendReply(RMLDB_REPLY_OK);
		break;
	case RMLDB_CMD_RUN:
		rmldb_execution_type = RMLDB_RUN;	
		rmldb_sendReply(RMLDB_REPLY_OK);		
		break;
	default:
		rmldb_sendReply(RMLDB_REPLY_ERROR);
		break;
	}
	rmldb_freeCommand(cmd);
}

int rmldb_sendEvent(char* event)
{
  return rmldb_socket_outln(rmldb_communicator.event_sock, event);
}

int rmldb_sendReply(char* reply)
{
  return rmldb_socket_outln(rmldb_communicator.reply_sock, reply);
}


#endif /* RML_DEBUG */
