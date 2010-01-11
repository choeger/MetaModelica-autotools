#include "rml.h"

/* vec-add.c */
RML_BEGIN_LABEL(RML__vector_5fadd)
{
	rml_uint_t nelts = 0;
	void *vec = rmlA0;
	void *data;
	nelts = RML_HDRSLOTS(RML_GETHDR(vec));
	{
		/* first alocate old_vector.length+1 then copy the old vector */
		struct rml_struct *vec_new = (struct rml_struct*)rml_prim_alloc(2+nelts, 2);
		void **vecp = vec_new->data;
		rml_uint_t i = 0;
		/* re-read after alloc, it may have been moved */
		vec = rmlA0;
		vec_new->header = RML_STRUCTHDR(nelts+1, 0);
		rmlA0 = RML_TAGPTR(vec_new);
		/* copy the old vector */
		for(i=0; i < nelts; i++)
			*vecp++ = RML_STRUCTDATA(vec)[i];
		/* set the last */
		*vecp = rmlA1;
	}
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* vec-add.c */
RML_BEGIN_LABEL(RML__vector_5farray)
{
	rml_uint_t nelts = 0;
	void *vec = rmlA0;
	void *data;
	nelts = RML_HDRSLOTS(RML_GETHDR(vec));
	{
		/* first alocate old_vector.length then copy the old vector */
		struct rml_struct *vec_new = (struct rml_struct*)rml_prim_alloc(1+nelts, 1);
		void **vecp = vec_new->data;
		rml_uint_t i = 0;
		vec_new->header = RML_STRUCTHDR(nelts, 0);
		rmlA0 = RML_TAGPTR(vec_new);
		/* copy the old vector */
		for(i = 0; i < nelts; i++)
			*vecp++ = RML_STRUCTDATA(vec)[i];
	}
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__vector_5fcopy)
{
	rml_uint_t nelts = 0;
	void *vec = rmlA0;
	void *data;
	nelts = RML_HDRSLOTS(RML_GETHDR(vec));
	{
		/* first alocate old_vector.length then copy the old vector */
		struct rml_struct *vec_new = (struct rml_struct*)rml_prim_alloc(1+nelts, 1);
		void **vecp = vec_new->data;
		rml_uint_t i = 0;
		vec_new->header = RML_STRUCTHDR(nelts, 0);
		rmlA0 = RML_TAGPTR(vec_new);
		/* copy the old vector */
		for(i = 0; i < nelts; i++)
			*vecp++ = RML_STRUCTDATA(vec)[i];
	}
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* vec-create.c */
RML_BEGIN_LABEL(RML__vector_5fcreate)
{
    /* then allocate and initialize the vector */
	rml_uint_t nelts = 0;
	nelts = RML_UNTAGFIXNUM(rmlA0);
    {
		struct rml_struct *vec = (struct rml_struct*)rml_prim_alloc(1+nelts, 2);
		void **vecp = vec->data;
		vec->header = RML_STRUCTHDR(nelts, 0);
		rmlA0 = RML_TAGPTR(vec);
		for(; nelts > 0; --nelts)
			*vecp++ = rmlA1;
    }
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* vec_length.c */
RML_BEGIN_LABEL(RML__vector_5flength)
{
    rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(RML_HDRSLOTS(RML_GETHDR(rmlA0))));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* vec_list.c */
RML_BEGIN_LABEL(RML__vector_5flist)
{
    /* Allocate a big blob for all the conses, i.e. 3 * #conses words,
     * and then initialize it.
     */
    rml_uint_t nelts = RML_HDRSLOTS(RML_GETHDR(rmlA0));
    void **consp = (void**)rml_prim_alloc(3*nelts, 1);
    void **vecp = RML_STRUCTDATA(rmlA0) + nelts;
    void *a0 = RML_TAGPTR(&rml_prim_nil);
    /* XXX: we should build the list in address order */
    for(; nelts > 0; a0 = RML_TAGPTR(consp), consp += 3, --nelts) {
	consp[0] = RML_IMMEDIATE(RML_CONSHDR);
	consp[1] = *--vecp;
	consp[2] = a0;
    }
    rmlA0 = a0;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* vec_nth.c */
RML_BEGIN_LABEL(RML__vector_5fnth)
{
    void *vec = rmlA0;
    rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
    if( i >= RML_HDRSLOTS(RML_GETHDR(vec)) ) {
	RML_TAILCALLK(rmlFC);
    } else {
	rmlA0 = RML_STRUCTDATA(vec)[i];
	RML_TAILCALLK(rmlSC);
    }
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__vector_5fget)
{
    void *vec = rmlA0;
    rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
    if( i-1 >= RML_HDRSLOTS(RML_GETHDR(vec)) ) {
	RML_TAILCALLK(rmlFC);
    } else {
	rmlA0 = RML_STRUCTDATA(vec)[i-1];
	RML_TAILCALLK(rmlSC);
    }
}
RML_END_LABEL

/* vec-setnth.c */
RML_BEGIN_LABEL(RML__vector_5fsetnth)
{
	rml_uint_t nelts = 0;
	void *vec = rmlA0;
	void *data;
	rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
	nelts = RML_HDRSLOTS(RML_GETHDR(vec));
	if( i >= nelts ) 
	{
		RML_TAILCALLK(rmlFC);
	} 
	else 
	{
		/* first copy the old vector */
		struct rml_struct *vec_new = (struct rml_struct*)rml_prim_alloc(1+nelts, 3);
		void **vecp = vec_new->data;
		rml_uint_t idx = 0;
		/* re-read after alloc, it may have been moved */
		vec = rmlA0;
		vec_new->header = RML_STRUCTHDR(nelts, 0);
		rmlA0 = RML_TAGPTR(vec_new);
		for(idx=0; idx < nelts; idx++)
			*vecp++ = RML_STRUCTDATA(vec)[idx];
		RML_STRUCTDATA(rmlA0)[i] = rmlA2;
	}
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* vec-setnth.c */
RML_BEGIN_LABEL(RML__vector_5fupdate)
{
	rml_uint_t nelts = 0;
	void *vec = rmlA0;
	void *data;
	rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
	nelts = RML_HDRSLOTS(RML_GETHDR(vec));
	if( i-1 >= nelts ) 
	{
		RML_TAILCALLK(rmlFC);
	} 
	else 
	{
		/* first copy the old vector */
		struct rml_struct *vec_new = (struct rml_struct*)rml_prim_alloc(1+nelts, 3);
		void **vecp = vec_new->data;
		rml_uint_t idx = 0;
		/* re-read after alloc, it may have been moved */
		vec = rmlA0;
		vec_new->header = RML_STRUCTHDR(nelts, 0);
		rmlA0 = RML_TAGPTR(vec_new);
		for(idx=0; idx < nelts; idx++)
			*vecp++ = RML_STRUCTDATA(vec)[idx];
		RML_STRUCTDATA(rmlA0)[i-1] = rmlA2;
	}
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL
