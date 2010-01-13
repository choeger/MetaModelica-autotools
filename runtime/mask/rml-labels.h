/*
 * rml-labels.h
 * for `dispatching switches'
 */

#ifdef __STDC__
#define RML_GLUE(A,B) A##B
#else /* this may or may not work.. */
#define RML_GLUE(A,B) A/**/B
#endif

typedef struct rml_label {
#ifdef	RML_STATE_APTR
    const struct rml_label	*(*fun)(unsigned, struct rml_state* const);
#else	/*!RML_STATE_APTR*/
    const struct rml_label	*(*fun)(unsigned);
#endif	/*AGPTR*/
    unsigned			tag;
    unsigned			*mask;
} rml_label_t;

typedef const struct rml_label *rml_labptr_t;
#define RML_LABPTR(LABEL)	(&LABEL)
#define RML_LABVAL(LABEL)	RML_IMMEDIATE(&LABEL)

#ifdef	RML_STATE_APTR

#define RML_FORWARD_MODULE(MODULE)	static const rml_label_t *MODULE(unsigned, struct rml_state* const)
#define RML_BEGIN_MODULE(MODULE)	static const rml_label_t *MODULE(unsigned switch_tag, struct rml_state * const rmlState)
#define RML_END_MODULE		/*empty*/
#define RML_APPLY(LABPTR)	((*((LABPTR)->fun))((LABPTR)->tag, rmlState))

#else	/*!RML_STATE_APTR*/

#ifdef	RML_STATE_LPTR

#define RML_FORWARD_MODULE(MODULE)	static const rml_label_t *MODULE(unsigned)
#define RML_BEGIN_MODULE(MODULE)	static const rml_label_t *MODULE(unsigned switch_tag) { struct rml_state * const rmlState = &rml_state;
#define RML_END_MODULE		}
#define RML_APPLY(LABPTR)	((*((LABPTR)->fun))((LABPTR)->tag))

#else	/*!RML_STATE_LPTR*/

#define RML_FORWARD_MODULE(MODULE)	static const rml_label_t *MODULE(unsigned)
#define RML_BEGIN_MODULE(MODULE)	static const rml_label_t *MODULE(unsigned switch_tag)
#define RML_END_MODULE		/*empty*/
#define RML_APPLY(LABPTR)	((*((LABPTR)->fun))((LABPTR)->tag))

#endif	/*RML_STATE_LPTR*/
#endif	/*RML_STATE_APTR*/

/* used internally in librml.a only */
extern const unsigned rml_internal_mask;
#define RML_FORWARD_LABEL(LABEL)	const rml_label_t LABEL
#define RML_BEGIN_LABEL(LABEL)\
RML_FORWARD_MODULE(RML_GLUE(module__,LABEL));\
const rml_label_t LABEL = {RML_GLUE(module__,LABEL),0,(unsigned*)&rml_internal_mask};\
RML_BEGIN_MODULE(RML_GLUE(module__,LABEL))
#define RML_END_LABEL		RML_END_MODULE
#define RML_TAILCALLK(KONT)	return (rml_labptr_t)RML_FETCH((KONT))

/* memory allocation */
extern void rml_prim_gc(rml_uint_t, rml_uint_t);
#define RML_ALLOC(VAR,NWORDS,NARGS,TAGLABEL) do{(VAR)=(void*)rml_young_next;if((rml_young_next=(void**)(VAR)+(NWORDS))>=rml_young_limit){gc_nwords=(NWORDS);gc_nargs=(NARGS);switch_tag=TAGLABEL;goto collect;}}while(0)
