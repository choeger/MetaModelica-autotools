
/* handler.h */
#ifndef HANDLER_H_
#define HANDLER_H_

extern int   aaerror(char*);
extern int   aaparse(void);
extern int   aadebug;

#ifdef RML_DEBUG

#if defined(__MINGW32__) || defined(_MSC_VER)

#if defined(__MINGW32__) /* ********** MINGW32 stuff ******/
#include <signal.h>
#endif

#else /***************** unix stuff ***************/
#include <signal.h>
#endif /* unix vs windows stuff */

/* adrpo - debugger global variables */
extern int   rmldb_cmd_port;    /* listen for commands from the client on this port */
extern int   rmldb_reply_port;  /* listen on this port and send synchronous command replys to the client */
extern int   rmldb_event_port;  /* listen on this port and send event to the client */
extern int   rmldb_signal_port; /* listen on this post and wait for async suspend signals from the client */

typedef struct rmldb_communication
{
	int   cmd_bind_sock;     /* bind and accept on this socket */
	int   cmd_sock;          /* listen for commands on this socket */
	int   reply_bind_sock;   /* bind and accept on this socket */
	int   reply_sock;        /* send reply of commands to this socket */	
	int   event_bind_sock;   /* bind and accept on this socket */
	int   event_sock;        /* send output to this port */
	int   signal_bind_sock;  /* bind and accept on this socket */
	int   signal_sock;       /* listen to async events from client */
} rmldb_communication_type;

extern rmldb_communication_type rmldb_communicator;

extern int rmldb_last_command;
extern int rmldb_startSignalHandler(void* arg);
extern int rmldb_endSignalHandler(void);
extern int rmldb_interpretCommand(void);
extern int rmldb_executeCommand(rmldb_command_type* cmd);
extern int rmldb_sendEvent(char* event);
extern int rmldb_sendReply(char* reply);

#endif /* RML_DEBUG */

#endif /*HANDLER_H_*/
