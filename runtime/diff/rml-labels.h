/*
 * rml-labels.h
 * for `differential labels'
 */

#ifdef __STDC__
#define RML_GLUE(A,B) A##B
#else /* this may or may not work.. */
#define RML_GLUE(A,B) A/**/B
#endif

typedef const struct rml_label *rml_labptr_t;
typedef struct rml_label {
    const struct rml_label	*(*fun)(rml_labptr_t
#ifdef	RML_STATE_APTR
					,struct rml_state* const
#endif	/*APTR*/
					);
} rml_label_t;

#define RML_LABPTR(LABEL)	(&LABEL)
#define RML_LABVAL(LABEL)	RML_IMMEDIATE(&LABEL)

#define RML_LABTAB_SETUP(MODFUN)	/*empty*/
#define RML_LABTAB_ENTRY(LABEL,MODFUN)	extern const rml_label_t LABEL;

#ifdef	RML_STATE_APTR

#define RML_FORWARD_MODULE(MODULE)	rml_labptr_t MODULE(rml_labptr_t, struct rml_state* const)
#define RML_BEGIN_MODULE(MODULE)	rml_labptr_t MODULE(rml_labptr_t theLabel, struct rml_state * const rmlState)
#define RML_END_MODULE		/*empty*/
#define RML_APPLY(LABPTR)	((*((LABPTR)->fun))((LABPTR), rmlState))

#else	/*!RML_STATE_APTR*/

#ifdef	RML_STATE_LPTR

#define RML_FORWARD_MODULE(MODULE)	rml_labptr_t MODULE(rml_labptr_t)
#define RML_BEGIN_MODULE(MODULE)	rml_labptr_t MODULE(rml_labptr_t theLabel) { struct rml_state * const rmlState = &rml_state;
#define RML_END_MODULE		}
#define RML_APPLY(LABPTR)	((*((LABPTR)->fun))((LABPTR)))

#else	/*!RML_STATE_LPTR*/

#define RML_FORWARD_MODULE(MODULE)	rml_labptr_t MODULE(rml_labptr_t)
#define RML_BEGIN_MODULE(MODULE)	rml_labptr_t MODULE(rml_labptr_t theLabel)
#define RML_END_MODULE		/*empty*/
#define RML_APPLY(LABPTR)	((*((LABPTR)->fun))((LABPTR)))

#endif	/*RML_STATE_LPTR*/
#endif	/*RML_STATE_APTR*/

/* used internally in librml.a only */
#define RML_FORWARD_LABEL(LABEL)	const rml_label_t LABEL
#define RML_BEGIN_LABEL(LABEL)\
static RML_FORWARD_MODULE(RML_GLUE(module__,LABEL));\
const rml_label_t LABEL = {RML_GLUE(module__,LABEL)};\
static RML_BEGIN_MODULE(RML_GLUE(module__,LABEL))
#define RML_END_LABEL		RML_END_MODULE
#define RML_TAILCALLK(KONT)	return (rml_labptr_t)RML_FETCH((KONT))

/* memory allocation */
extern void rml_prim_gc(rml_uint_t, rml_uint_t);
#define RML_ALLOC(VAR,NWORDS,NARGS,TAGLABEL) do{(VAR)=(void*)rml_young_next;if((rml_young_next=(void**)(VAR)+(NWORDS))>=rml_young_limit){gc_nwords=(NWORDS);gc_nargs=(NARGS);switch_tag=TAGLABEL;goto collect;}}while(0)
