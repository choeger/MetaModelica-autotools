/* config.h for MIPS / Irix 6 / cc */

/* the following section was generated by repchk */
typedef int rml_sint_t;
typedef unsigned int rml_uint_t;
#define RML_LOG2_SIZE_INT 2
#define RML_SIZE_INT 4
#define RML_SIZE_DBL 8
#define RML_DBL_PAD
#define RML_DBL_STRICT
/* end of repchk-generated section */

#define RML_CLOCK_TIMES

#define RML_ALLOCA_H

#define rml_setjmp	setjmp
#define rml_longjmp	longjmp

/* preliminary tuning parameters taken from mips-ultrix-cc/config.h */
#ifdef	RML_PLAIN
#define RML_YOUNG_SIZE	(108*1024)
#endif
#ifdef	RML_SWITCH
#define RML_YOUNG_SIZE	(80*1024)
#endif

#ifdef	RML_PLAIN
#define RML_STATE_JOIN
#define RML_STATE_APTR
#endif
#ifdef	RML_SWITCH
#define RML_STATE_JOIN
#define RML_STATE_APTR
#endif
