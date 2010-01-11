/* defines.c */
#include <stdio.h>
#include <string.h>
#include "rml.h"

char rml_debug_enabled = 0;
char rml_trace_enabled = 0;

/* p_nil.c */
const struct rml_header rml_prim_nil = { RML_NILHDR };

/* p_unwind.c */
void rml_prim_unwind_(void **saveTP)	/* PRE: rmlTP < saveTP */
{
    void **TP = rml_state_TP;
    do { RML_GETHDR(*TP) = RML_UNBOUNDHDR; } while( ++TP < saveTP );
    rml_state_TP = TP;
}

/*
 * p_equal.c -- implements polymorphic equality for RML
 * (This is the reason why reference nodes must still be distinguishable
 * from all other values.)
 */
void *rml_prim_equal(void *p, void *q)
{
  tail_recur:
    /* INV: ISIMM(p) <==> ISIMM(q) */
    if( p == q ) {
	/* Identical objects are always equal. */
	return RML_TRUE;
    } else if( RML_ISIMM(p) ) {
	/* Different immediate values. */
	return RML_FALSE;
    } else {
	/* Non-identical boxed values. */
	rml_uint_t phdr = RML_GETHDR(p);
	rml_uint_t qhdr = RML_GETHDR(q);

	if( phdr == qhdr ) {
	    if( phdr == RML_REALHDR ) {
		return (rml_prim_get_real(p) == rml_prim_get_real(q))
		    ? RML_TRUE
		    : RML_FALSE;
	    } else if( RML_HDRISSTRING(phdr) ) {
		if( !memcmp(RML_STRINGDATA(p), RML_STRINGDATA(q), RML_HDRSTRLEN(phdr)) )
		    return RML_TRUE;
		else
		    return RML_FALSE;
	    } else if( RML_HDRISSTRUCT(phdr) ) {
		rml_uint_t slots = RML_HDRSLOTS(phdr);
		void **pp = RML_STRUCTDATA(p);
		void **qq = RML_STRUCTDATA(q);
		if( slots == 0 )
		    return RML_TRUE;
		while( --slots > 0 )
		    if( rml_prim_equal(*pp++, *qq++) == RML_FALSE )
			return RML_FALSE;
		p = *pp;
		q = *qq;
		goto tail_recur;
	    } else {
		/* Non-identical reference nodes. */
		return RML_FALSE;
	    }
	} else {
	    /* Different sized strings, different constructors of some datatype,
	     * or reference nodes with different instantiation states.
	     */
	    return RML_FALSE;
	}
    }
}


RML_BEGIN_LABEL(RML__if_5fexp)
{
	if (RML_UNTAGFIXNUM(rmlA0))
		rmlA0 = rmlA1;
	else
		rmlA0 = rmlA2;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* p_clock.c
 * Extremely system dependent implementation of a simple
 * timer facility, analogous to ANSI-C's clock().
 */
#ifdef RML_CLOCK_RUSAGE
#include <sys/time.h>
#include <sys/resource.h>
extern int getrusage(int, struct rusage*);

unsigned long rml_prim_clock(void)
{
    struct rusage ru;
    unsigned long usecs;
    double scale;

    (void)getrusage(RUSAGE_SELF, &ru);
    usecs = ru.ru_utime.tv_sec * 1000000 + ru.ru_utime.tv_usec
	  + ru.ru_stime.tv_sec * 1000000 + ru.ru_stime.tv_usec;
    scale = (double)RML_CLOCKS_PER_SEC / 1000000.0;
    return (unsigned long)((double)usecs * scale);
}
#endif

#ifdef RML_CLOCK_TIMES
#include <sys/times.h>
#include <limits.h>
#include <time.h>	/* for glibc2 */
#include <unistd.h> /* for sysconf */
extern clock_t times(struct tms*);

unsigned long rml_prim_clock(void)
{
    struct tms tms;
    double scale;

    (void)times(&tms);
#ifdef CLK_TCK
    scale = (double)RML_CLOCKS_PER_SEC / (double)CLK_TCK;
#else /* use new sysconf(_SC_CLK_TCK) */
    scale = (double)RML_CLOCKS_PER_SEC / (double)sysconf(_SC_CLK_TCK);
#endif
    return (unsigned long)((double)(tms.tms_utime + tms.tms_stime) * scale);
}
#endif

/* misc_clock.c */
#ifdef RML_CLOCK_PARIX
#include <sys/time.h>

unsigned long rml_prim_clock(void)
{
    double scale = (double)RML_CLOCKS_PER_SEC / (double)CLOCK_TICK;
    return (unsigned long)((double)TimeNow() * scale);
}
#endif

#ifdef RML_CLOCK_ANSIC
#include <time.h>

unsigned long rml_prim_clock(void)
{
    double scale = (double)RML_CLOCKS_PER_SEC / (double)CLOCKS_PER_SEC;
    return (unsigned long)((double)clock() * scale);
}
#endif


RML_BEGIN_LABEL(RML__clock)
{
    rmlA0 = rml_prim_mkreal((double)rml_prim_clock() / (double)RML_CLOCKS_PER_SEC);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* misc_print.c */
RML_BEGIN_LABEL(RML__print)
{
    void *str = rmlA0;
    fwrite(RML_STRINGDATA(str), RML_HDRSTRLEN(RML_GETHDR(str)), 1, stdout);
	fflush(stdout);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* misc_tick.c */
RML_BEGIN_LABEL(RML__tick)
{
    static rml_sint_t counter;
    rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(counter));
    ++counter;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


void rmldb_var_print(void *p)
{
	/* printf("[%p]", p); */
	if (!p) { printf ("NIL"); fflush(stdout); return; }
	if( RML_ISIMM(p) ) 
	{
		printf ("%d", RML_UNTAGFIXNUM(p));    
	} 
	else 
	{
		rml_uint_t phdr = RML_GETHDR(p);            
		if( phdr == RML_REALHDR ) 
		{
			printf ("%f", rml_prim_get_real(p));
			fflush(stdout);
		} else 
			if( RML_HDRISSTRING(phdr) ) 
			{
				printf ("\"%s\"", RML_STRINGDATA(p));
				fflush(stdout);
				/* use if neccesarry RML_HDRSTRLEN(phdr) */
			} else 
				if( RML_HDRISSTRUCT(phdr) ) 
				{
					rml_uint_t slots = RML_HDRSLOTS(phdr);
					rml_uint_t constr = RML_HDRCTOR(phdr);
					void **pp = NULL;
					if (slots == 0)
					{
						printf ("{S(%d)[%d]=NIL}", constr, slots);
						fflush(stdout);
						return;
					}
					
					printf ("S(%d)[%d](", constr, slots);

					pp = RML_STRUCTDATA(p);
					fflush(stdout);
					// function definition
					if ((constr == 64 || constr==13) &&
						slots > 1000000) return;
					if( slots != 0 )
					{
						// printf ("\n\t"); 
						while( --slots > 0 )
						{
							rmldb_var_print(*pp++);
							printf (",");
							fflush(stdout);
						}
						p = *pp; 
						rmldb_var_print(*pp); printf (")"); fflush(stdout);
						// goto tail_recur_debug;  
					}					    
				} 
				else 
				{
					printf ("UNKNOWN"); fflush(stdout);
				}
	}
}
