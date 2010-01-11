/*
* main.c -- provide a dummy main for RML -- call procedure main in module Main
*/
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include "rml.h"

void rml_show_help(char *program, FILE* file)
{
  fprintf(file, "Usage: %s [runtime_options] program_options\n", program);
  fprintf(file, "where [runtime_options] are:\n");
  fprintf(file, "-log                   prints runtime log information at the program exit\n");
  fprintf(file, "-gcgag                 prints garbage collector log information at the program exit\n");
  fprintf(file, "-bench                 prints running time and other log information at the program exit\n");
  fprintf(file, "-no-stack-check        instructs the runtime no to check for stack overflow\n");
  fprintf(file, "-stack-size=<size>     instructs the runtime to alloc a stack of specified size\n");
  fprintf(file, "-young-size=<size>     instructs the runtime to alloc a young size of specified size\n");
  fprintf(file, "*************************** TRACING COMMANDS *************************\n");
  fprintf(file, "-trace                 prints all the function names during execution; default to 'no'; NOTE: compiled with -ftrace\n");
  fprintf(file, "-no-trace              disable printing of all the function names; default to 'yes'; NOTE: compiled with -ftrace\n");
  fprintf(file, "************************** DEBUGGING COMMANDS *************************\n");
  fprintf(file, "NOTE: if you don't specify one of the ports below the executable will run without any debugging.\n");  
  fprintf(file, "-dbgCmdPort=<port>     open the server localhost:CmdPort and listen for commands from the client\n");
  fprintf(file, "-dbgReplyPort=<port>   open the server localhost:ReplyPort and send reply to commands from the client\n");  
  fprintf(file, "-dbgEventPort=<port>   open the server localhost:EventPort and send async events the client\n");
  fprintf(file, "-dbgSignalPort=<port>  open the server localhost:SignalPort and listen to ansync events from the client\n");  
  fprintf(file, "-dbgSocket             debug the socket communication.\n");
	fprintf(file, "-debugAll              dumps all the calls and the values of variables to standard output; NOTE: can be extremly large\n");
  fprintf(file, "-help                  prints the help and exits\n");
}

static void rml_prim_argv(int argc, char **argv)
{
#if	defined(RML_STATE_APTR) || defined(RML_STATE_LPTR)
  struct rml_state *rmlState = &rml_state;
#endif	/*RML_STATE_APTR || RML_STATE_LPTR*/
  rmlA0 = RML_TAGPTR(&rml_prim_nil);
  while( --argc >= 0 ) {
    {
      char *s = argv[argc];
      rml_uint_t len = strlen(s);
      struct rml_string *str = rml_prim_mkstring(len, 1);
      memcpy(str->data, s, len);
      str->data[len] = '\0';
      rmlA1 = RML_TAGPTR(str);
    }
    {
      struct rml_struct *cons = (struct rml_struct*)rml_prim_alloc(3, 2);
      cons->header = RML_CONSHDR;
      cons->data[0] = rmlA1;
      cons->data[1] = rmlA0;
      rmlA0 = RML_TAGPTR(cons);
    }
  }
}

int rmldb_debug_start()
{
#if defined(RML_DEBUG)
  /* stop and ask for commands here; this is a good time to set breakpoints */
  rmldb_init();
#endif /* defined(RML_DEBUG) */
}

int rmldb_debug_end()
{
#ifdef RML_DEBUG
    rmldb_end(); /* cleanup the debugger */
#endif /* RML_DEBUG */
}

static unsigned long my_atoul(const char *nptr)
{
  unsigned long res;
  char *endptr;

  res = strtoul(nptr, &endptr, 0);
  if( res != ULONG_MAX ) {
    switch( *endptr ) {
    case 'M':
      res *= 1024;
      /*FALLTHROUGH*/
    case 'K':
      res *= 1024;
      /*FALLTHROUGH*/
    case '\0':
      return res;
    }
  }
  return ULONG_MAX;
}

int main(int argc, char **argv)
{
  char* program = argv[0];
#ifdef RML_DEBUG
  rmldb_execution_startup_type = RMLDB_STEP;
#endif /* RML_DEBUG */
  for(++argv, --argc; argc > 0 && argv[0][0] == '-';) {
    char *arg = &argv[0][1];
    ++argv, --argc;
    if( strcmp(arg, "log") == 0 ) {
      rml_flag_log = 1;
      rml_flag_gclog = 1;
      rml_flag_no_stack_check = 0;
      continue;
    } else if( strcmp(arg, "gcgag") == 0 ) {
      rml_flag_gclog = 1;
      continue;
    } else if( strcmp(arg, "bench") == 0 ) {
      rml_flag_bench = 1;
      rml_flag_no_stack_check = 1;
      rml_flag_gclog = 1;
      rml_clock_start = rml_prim_clock();
      continue;
    } else if( strcmp(arg, "no-stack-check") == 0 ) {
      rml_flag_no_stack_check = 1;
      continue;
    } else if( strncmp(arg, "stack-size=", 11) == 0 ) {
      if( (rml_stack_size = my_atoul(arg+11)) == ULONG_MAX ) {
        fprintf(stderr, "Illegal argument: -stack-size=%s\n", arg);
        rml_stack_size = 0;
        rml_exit(1);
      }
      continue;
    }  else if( strncmp(arg, "young-size=", 11) == 0 ) {
      if( (rml_young_size = my_atoul(arg+11)) == ULONG_MAX ) {
        fprintf(stderr, "Illegal argument: -young-size=%s\n", arg);
        fprintf(stderr, "Falback to: -young-size=%s\n", RML_YOUNG_SIZE);
        rml_young_size = RML_YOUNG_SIZE;
        continue;
      }
      continue;
    }  else if( strncmp(arg, "dbgCmdPort=", 11) == 0 ) {
#ifdef RML_DEBUG
      if( (rmldb_cmd_port = my_atoul(arg+11)) == ULONG_MAX ) 
      {
        fprintf(stderr, "Illegal argument: -%s\n", arg);
        fprintf(stderr, "Exiting");
        rml_show_help(program, stderr);
        rml_exit(2);
      }
#endif /* RML_DEBUG */
      continue;
    }  else if( strncmp(arg, "dbgReplyPort=", 13) == 0 ) {
#ifdef RML_DEBUG
      if( (rmldb_reply_port = my_atoul(arg+13)) == ULONG_MAX ) 
      {
        fprintf(stderr, "Illegal argument: -%s\n", arg);
        fprintf(stderr, "Exiting");
        rml_show_help(program, stderr);
        rml_exit(2);
      }
#endif /* RML_DEBUG */
      continue;
    }  else if( strncmp(arg, "dbgEventPort=", 13) == 0 ) {
#ifdef RML_DEBUG
      if( (rmldb_event_port = my_atoul(arg+13)) == ULONG_MAX ) 
      {
        fprintf(stderr, "Illegal argument: -%s\n", arg);
        fprintf(stderr, "Exiting");
        rml_show_help(program, stderr);
        rml_exit(2);
      }
#endif /* RML_DEBUG */
      continue;
    }  else if( strncmp(arg, "dbgSignalPort=", 14) == 0 ) {
#ifdef RML_DEBUG
      if( (rmldb_signal_port = my_atoul(arg+14)) == ULONG_MAX ) 
      {
        fprintf(stderr, "Illegal argument: -%s\n", arg);
        fprintf(stderr, "Exiting");
        rml_show_help(program, stderr);
        rml_exit(2);
      }
#endif /* RML_DEBUG */
      continue;
    } else if( strcmp(arg, "dbgSocket") == 0 ) {
#ifdef RML_DEBUG
      rmldb_socket_debug = 1;
#endif /* RML_DEBUG */
      continue;
    } else if( strcmp(arg, "trace") == 0 ) {
      rml_trace_enabled = 1;
      continue;
    } else if( strcmp(arg, "no-trace") == 0 ) {
      rml_trace_enabled = 0;
      continue;
    } else if( strcmp(arg, "dbgCalls") == 0 ) {
#ifdef RML_DEBUG
      rmldb_execution_startup_type = RMLDB_TRACE_CALLS;
#else
      fprintf(stderr, "You have to compile your program in debug mode for this flag.\n");
#endif /* RML_DEBUG */
	    continue;
	} else if( strcmp(arg, "debugAll") == 0 ) {
#ifdef RML_DEBUG
	    rmldb_execution_startup_type = RMLDB_TRACE_ALL;
#else
		  fprintf(stderr, "You have to compile your program in debug mode for this flag.\n");
#endif /* RML_DEBUG */
	    continue;
    } else if( strcmp(arg, "help") == 0 ) {
      rml_show_help(program, stdout);
      rml_exit(0);
    } else if( strcmp(arg, "-") == 0 ) {
      break;
    } else {
      fprintf(stderr, "Illegal argument: -%s\n", arg);
      rml_show_help(program, stderr);
      rml_exit(1);
    }
  }	

#ifdef RML_DEBUG
  /* if we have no ports set, don't do debugging */
  if (rmldb_cmd_port != -1 || 
      rmldb_reply_port != -1 || 
      rmldb_event_port != -1 || 
      rmldb_signal_port != -1 ||
      rmldb_execution_startup_type == RMLDB_TRACE_ALL)
  {
    rml_debug_enabled = 1;
  }
#endif /* RML_DEBUG */  
  Main_5finit();

  rmldb_debug_start();

  rml_prim_argv(argc, argv);
  if( rml_prim_once(RML_LABPTR(Main__main)) == 0 ) 
  {
    rmldb_debug_end();	
    fprintf(stderr, "Execution failed!\n");
    rml_exit(1);
  } 
  else 
  {
  	rmldb_debug_end();
  	rml_exit(0);
  }
  /*NOTREACHED*/
  return 1;
}
