/* config.h for RS6000 / AIX 4.1 / gcc */

/* the following section was generated by repchk */
typedef int rml_sint_t;
typedef unsigned int rml_uint_t;
#define RML_LOG2_SIZE_INT 2
#define RML_SIZE_INT 4
#define RML_SIZE_DBL 8
/* end of repchk-generated section */

#define	RML_CLOCK_RUSAGE	/* has times() too but not CLK_TCK */

#define RML_ALLOCA_BUILTIN

#define rml_setjmp	_setjmp
#define rml_longjmp	_longjmp

/* preliminary tuning options */
#define RML_YOUNG_SIZE	(84*1024)
#define RML_STATE_JOIN
#define RML_STATE_APTR

#define RML_GCCGOTO
#define RML_GCCGOTO_NOSHIFT