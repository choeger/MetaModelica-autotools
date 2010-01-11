/* p-init.c
 * initialization routines invoked by compiled RML code
 */
#include <stdio.h>
#include <stdlib.h>	/* abort() */
#include <setjmp.h>
#include "rml.h"

extern void rml_gcinit(void);

void RML_5finit(void)
{
    static int init_done = 0;
    if( !init_done ) {
	    rml_gcinit();
	    init_done = 1;
    }
}

void rml_gval_init(struct rml_gval *first)
{
    struct rml_gval *prev = 0, *here = first;

    /*
     * Follow the "next" chain until we find an initialized node or a cycle.
     */
    for(;;) {
	struct rml_gval *next = here->next;
	if( next == 0 )		/* is here initialized? */
	    break;
	if( next == here ) {	/* is here busy? (here->next == next) */
	    fprintf(stderr, "Initialization error: global value dependency cycle detected:\n");
	    fprintf(stderr, "%s", here->name);
	    while( prev ) {
		fprintf(stderr, " <-- %s", prev->name);
		if( prev == here )
		    break;
		prev = prev->u.prev;
	    }
	    fprintf(stderr, "\n");
	    exit(1);
	}
	/* here is neither initialized nor busy */
	here->u.prev = prev;
	prev = here;
	here = next;
	prev->next = prev;
    }

    /*
     * "here" is an initialized node.
     * Copy "here"'s value to every node in the "prev" chain.
     */
    {
	void *value = here->u.value;

	while( prev ) {
	    here = prev;
	    prev = prev->u.prev;
	    here->u.value = value;
	    here->next = 0;
	}
    }
}


#ifdef RML_DEBUG_
typedef struct simulation
{
    void *SC, *FC, *F;
} simulation_t;
simulation_t sim[10000];
int simIndex = 0;
#endif /* RML_DEBUG */

/* p_motor.c */
/*
 * Run labels "for ever".
 */
void rml_prim_motor(rml_labptr_t f)
{
    int i;
#if	defined(RML_STATE_APTR) || defined(RML_STATE_LPTR)
    struct rml_state *rmlState = &rml_state;
#endif	/*RML_STATE_APTR || RML_STATE_LPTR*/
    if( rml_flag_no_stack_check )
    {	/* run a simple and fast motor */
    	for(;;)
        {
    	    f = RML_APPLY(f);
#ifdef RML_DEBUG_
            rmldb_pop_stack_frame();
#endif  /* RML_DEBUG */
        }
    } else {			/* run a careful and logging motor */
#ifdef	RML_MORE_LOGGING
	const char *previous_module = (const char*)0;
	unsigned char previous_known = 0;
#endif	/*RML_MORE_LOGGING*/
	for(;;) {
    /*
     * turn on the trace if we're almost out of stack!
     */
	    if( (void**)rmlSP < rmlSPMIN )
			rmlSPMIN = (void**)rmlSP;
      if( (void**)rmlSP < &rml_stack[32*4] ) {
        rml_trace_enabled = 1;
      }
	    /*
	     * Add a small buffer zone at the stack bottom, to reduce
	     * the risk of writes outside the stack bounds.
	     */
	    if( (void**)rmlSP < &rml_stack[32] ) {
			fprintf(stderr, "Stack overflow!\n");
			rml_exit(1);
	    }
	    /* printf("[sp: %d] ", (void**)rmlSP-&rml_stack[rml_stack_size]); */
#ifdef RML_DEBUG_
        sim[simIndex].FC = RML_FETCH(rmlFC);
        sim[simIndex].SC = RML_FETCH(rmlSC);
        sim[simIndex].F = RML_LABVAL(f);
        fprintf(stderr, "push SC:%p, FC:%p, F:%p, index:%d\n", RML_FETCH(rmlSC), RML_FETCH(rmlFC), RML_LABVAL(f), simIndex);
        simIndex++;
#endif /* RML_DEBUG */
	    f = RML_APPLY(f);
#ifdef RML_DEBUG_
        for(i=0; i < simIndex; i++)
            if (sim[i].FC == RML_LABVAL(f) || sim[i].SC == RML_LABVAL(f) || sim[i].F == RML_LABVAL(f))
            {
                int j;
                char what = 0;
                if (sim[i].FC == RML_LABVAL(f)) what = 0;
                if (sim[i].SC == RML_LABVAL(f)) what = 1;
                rmldb_pop_stack_frame();
                simIndex = i;
                fprintf(stderr, "pop  FUNCTION:%p, index:%d %s\n", RML_LABVAL(f), simIndex, what?"success":"failure");
                break;
            }
        fflush(stderr);
#endif  /* RML_DEBUG */
	    ++rml_call_count;
#ifdef	RML_MORE_LOGGING
	    if( previous_module == rml_latest_module ) {
		++rml_intra_calls;
		if( previous_known )
		    ++rml_intra_known_calls;
	    } else {
			++rml_inter_calls;
			if( previous_known )
		    	++rml_inter_known_calls;
	    }
	    previous_module = rml_latest_module;
	    previous_known = rml_latest_known;
#endif	/*RML_MORE_LOGGING*/
	}
    }
    /*NOTREACHED*/
}

/* p_once.c */
RML_BEGIN_LABEL(rml_sclam_once)
{
    jmp_buf *there = (jmp_buf*)RML_FETCH(RML_OFFSET(rmlSC, 1));
    longjmp(*there, 2);
    return 0;
}
RML_END_LABEL

RML_BEGIN_LABEL(rml_fclam_once)
{
    jmp_buf *there = (jmp_buf*)RML_FETCH(RML_OFFSET(rmlFC, 2));
    longjmp(*there, 1);
    return 0;
}
RML_END_LABEL

int rml_prim_once(rml_labptr_t f)
{
    void *origSP, *origSC, *origFC;
    jmp_buf here;
    int status;

    origSC = rml_state_SC;
    origFC = rml_state_FC;
    origSP = rml_state_SP;

    RML_STORE(RML_OFFSET(origSP, -1), (void*)here);
    RML_STORE(RML_OFFSET(origSP, -2), RML_LABVAL(rml_sclam_once));
    RML_STORE(RML_OFFSET(origSP, -3), RML_LABVAL(rml_fclam_once));
    rml_state_SC = RML_OFFSET(origSP, -2);
    rml_state_FC = RML_OFFSET(origSP, -3);
    rml_state_SP = RML_OFFSET(origSP, -3);

    /* ANSI-C 4.6.1.1, 3.6.4, and 3.6.5 imply that one cannot
     * portably assign the value of "setjmp()" to a variable.
     * Below, I'd really like to write:
     *		if( (status = setjmp(here)) == 0 )
     *		    rml_prim_motor(f);
     * but instead I "switch()" on the different cases.
     * (Note: I have yet to find an implementation where
     * the code above actually breaks.)
     */
    switch( setjmp(here) ) {
      case 0:		/* starting */
		rml_prim_motor(f);
		/*FALLTHROUGH*/
      case 1:		/* rml_fclam_once: failure */
		status = 1;
		break;
      default:		/* rml_sclam_once: success */
		status = 2;
    }
    /* According to ANSI-C 4.6.2.1, as I haven't changed the orig*
     * variables between the setjmp() and longjmp() calls, they have
     * their correct values now, even though they aren't `volatile'.
     */
    rml_state_SC = origSC;
    rml_state_FC = origFC;
    rml_state_SP = origSP;

    return status - 1;	/* 0 on failure, 1 on success */
}
