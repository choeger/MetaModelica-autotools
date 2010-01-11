%{

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "rml.h"

#include "parse-command.h"

#ifdef RML_DEBUG

#ifndef _PROTOTYPE
 #ifdef __STDC__
  #define _PROTOTYPE(func, args) func args
 #else
  #define _PROTOTYPE(func, args) func()
 #endif
#endif

/*
#ifdef SMALL_BUF
#undef YY_READ_BUF_SIZE
#define YY_READ_BUF_SIZE 512
#endif
*/
/* We want to define our own yywrap. */
#undef aawrap
_PROTOTYPE(int aawrap, (void));

/* Have input call the following function. */
#undef  YY_INPUT
#define YY_INPUT(buf,result,max_size) \
		socket_input((char *)buf, &result, max_size)

static void socket_input (buf, result, max)
	char *buf;
	int  *result;
	int   max;
{	
    /* we are using the debugger over sockets */
	size_t nb_bytes;                                                 
	nb_bytes = _rmldb_socket_recv(rmldb_communicator.cmd_sock, (void*)buf, (size_t)max);           
	if ((nb_bytes == 0) || (nb_bytes == -1)) 
	{ 
		*result = YY_NULL;
		/* exit if the other part closed the socket */
		if (rmldb_socket_debug)
	      	fprintf (stderr, "%s Error! Receiving from socket failed: %s. Received: %d bytes.\n", 
	      		RMLDB_SOCKET_PROMPT, strerror(_rmldb_socket_errorno), nb_bytes);
		if (nb_bytes == -1 ) exit (1);
	}   
	else 
	{ 
		*result = nb_bytes;
	}
	return;
}

#endif /* RML_DEBUG */

%}


%option 8bit
%option caseless
%option nodefault
%option outfile = "scan-command.c"
%option prefix = "aa"
%option always-interactive
/* %option debug */

%%



^"$b"   { return 	T_BREAKPOINT;        			}
^"$d"   { return 	T_DELETE_BREAKPOINT; 			}
^"$bm"  { return 	T_FUNCTION_BREAKPOINT;   		}
^"$bmd" { return	T_DELETE_FUNCTION_BREAKPOINT;	}
^"$bs" 	{ return	T_STRING_BREAKPOINT;   			}
^"$bsd" { return	T_DELETE_STRING_BREAKPOINT;		}
^"$bf" 	{ return	T_FAILURE_BREAKPOINT;   		}
^"$bfd" { return	T_DELETE_FAILURE_BREAKPOINT;	}
^"$c"   { return	T_CLEAR_BREAKPOINTS; 			}
^"$s"   { return	T_STEP;              			}
^"$o"   { return  	T_STEP_OVER;         			}
^"$n"   { return  	T_STEP_RETURN;         			}
^"$r"   { return 	T_RUN;   						}	
^"$k" 	{ return 	T_STACK;						}
^"$v" 	{ return 	T_VARIABLE_VALUE;	    		}
^"$vl"  { return 	T_LAZY_VARIABLE_VALUE;   		}
^"$fv"	{ return 	T_FRAME_VARIABLE_VALUE;			}
^"$fvl" { return 	T_FRAME_LAZY_VARIABLE_VALUE;	}	
^"$q" 	{ return 	T_QUIT;							}

\:				{ return T_COLON; }
\n              { return T_EOL; }
[ \t]+			{ /* Skip whitespace */ }

[[:alnum:]|"."|"'"|"_"|"*"]+ { 
	aalval.id = (void*)malloc(strlen(aatext)+1);
	if (aadebug) printf("FLEX:[ID:%s]", aatext); 
	strcpy((char*)aalval.id, aatext); 
	return T_ID; 
}


<<EOF>>			{ yyterminate(); }
.				{ if (aadebug) printf("FLEX:bad character %s:\n", aatext); /* yyterminate(); */ }
%%

int aawrap() { return 1; }
