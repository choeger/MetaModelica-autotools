/* config.h for RS6000 / PARIX 1.x / cc */

/* the following section was generated by repchk */
typedef int rml_sint_t;
typedef unsigned int rml_uint_t;
#define RML_LOG2_SIZE_INT 2
#define RML_SIZE_INT 4
#define RML_SIZE_DBL 8
/* end of repchk-generated section */

#define RML_DBL_STRICT	/* Parix complains about doubles crossing pages */

#define	RML_CLOCK_PARIX

#define	RML_ALLOCA_H

#define rml_setjmp	setjmp
#define rml_longjmp	longjmp

#ifdef	RML_PLAIN
#define RML_YOUNG_SIZE	(84*1024)
#endif
#ifdef	RML_SWITCH
#define RML_YOUNG_SIZE	(84*1024)
#endif

#ifdef	RML_PLAIN
#define RML_STATE_JOIN
#define RML_STATE_APTR
#endif
#ifdef	RML_SWITCH
#define RML_STATE_JOIN
#define RML_STATE_APTR
#endif
