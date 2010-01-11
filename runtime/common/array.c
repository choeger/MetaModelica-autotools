/* arr-add.c */
#include <stdio.h>
#include "rml.h"
/* use this define to have array behave as vectors
#define NON_DESTRUCTIVE_ARRAY_IMPLEMENTATION
*/

RML_BEGIN_LABEL(RML__array_5fadd)
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

/* arr-create.c */
RML_BEGIN_LABEL(RML__array_5fcreate)
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

/* arr-length.c */
RML_BEGIN_LABEL(RML__array_5flength)
{
    rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(RML_HDRSLOTS(RML_GETHDR(rmlA0))));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* arr-list.c */
RML_BEGIN_LABEL(RML__array_5flist)
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

/* arr-nth.c */
RML_BEGIN_LABEL(RML__array_5fnth)
{
    void *vec = rmlA0;
    rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
    if( i >= RML_HDRSLOTS(RML_GETHDR(vec)) ) 
	{
		/*
		fprintf(stderr, "trying to get %d of array of lenght %d", 
		i,
		RML_HDRSLOTS(RML_GETHDR(vec)));
		*/
	 RML_TAILCALLK(rmlFC);
    } else {
	rmlA0 = RML_STRUCTDATA(vec)[i];
	RML_TAILCALLK(rmlSC);
    }
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__array_5fget)
{
    void *vec = rmlA0;
    rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
    if( i-1 >= RML_HDRSLOTS(RML_GETHDR(vec)) ) 
	{
		/*
	    fprintf(stderr, "trying to get %d of array of lenght %d", 
		i,
		RML_HDRSLOTS(RML_GETHDR(vec)));
		*/
	 RML_TAILCALLK(rmlFC);
    } else {
	rmlA0 = RML_STRUCTDATA(vec)[i-1];
	RML_TAILCALLK(rmlSC);
    }
}
RML_END_LABEL

/* arr-setnth.c */
RML_BEGIN_LABEL(RML__array_5fsetnth)
{
#ifndef NON_DESTRUCTIVE_ARRAY_IMPLEMENTATION
	rml_uint_t nelts = 0;
	void *arr = rmlA0;
	void *data;
	rml_uint_t idx = 0;
	rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
	if( i >= RML_HDRSLOTS(RML_GETHDR(arr)) ) 
	{
		RML_TAILCALLK(rmlFC);
	} 

	/* update the array in place */
	RML_STRUCTDATA(arr)[i] = rmlA2;
	/* IF the object updated is not immediate add 
		* the array address to the roots to be swept for pointers
		* pointing in the newer generation
		*/
	if (!RML_ISIMM(rmlA2))
	{
		/* also check here if the array AND the element is in the young generation */
		/*
		printf("\n%p < %p < %p < %p\n", rml_young_region, RML_UNTAGPTR(rmlA0), RML_UNTAGPTR(rmlA2), rml_young_limit); 
		*/
		/* check if both are in the young generation */
		/*
		if(RML_UNTAGPTR(rmlA0) - (void*)(rml_young_region) >= 0 &&
			(void*)(rml_young_limit) - RML_UNTAGPTR(rmlA0) > 0 && 
			RML_UNTAGPTR(rmlA2) - (void*)(rml_young_region) >= 0 &&
			(void*)(rml_young_limit) - RML_UNTAGPTR(rmlA2) > 0)
		{
			rmlA0 = arr;
			RML_TAILCALLK(rmlSC);
		}
		*/
		/* also check here if the array is not already in the trail */
		for (idx = rml_array_trail_size; &rml_array_trail[idx] >= rmlATP; idx--)
		if (rml_array_trail[idx] == rmlA0) /* if found, do not add again */
		{
			rmlA0 = arr;
			RML_TAILCALLK(rmlSC);
		}
		/* add the address of the array into the roots to be
		taken into consideration at the garbage collection time */
		if( rmlATP == &rml_array_trail[0] ) 
		{
			(void)fprintf(stderr, "Array Trail Overflow!\n");
			rml_exit(1);
		}
		*--rmlATP = rmlA0;
	}
	rmlA0 = arr;
	RML_TAILCALLK(rmlSC);

#else /* non destructive array implementation (using copy) */

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

#endif
}
RML_END_LABEL

/* arr-update.c */
RML_BEGIN_LABEL(RML__array_5fupdate)
{
#ifndef NON_DESTRUCTIVE_ARRAY_IMPLEMENTATION
	rml_uint_t nelts = 0;
	void *arr = rmlA0;
	void *data;
	rml_uint_t idx = 0;
	rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
	if( i-1 >= RML_HDRSLOTS(RML_GETHDR(arr)) ) 
	{
		RML_TAILCALLK(rmlFC);
	} 
	/* update the array in place */
	RML_STRUCTDATA(arr)[i-1] = rmlA2;
	/* IF the object updated is not immediate add 
		* the array address to the roots to be swept for pointers
		* pointing in the newer generation
		*/
	if (!RML_ISIMM(rmlA2))
	{
		/* also check here if the array AND the element is in the young generation */
		/*
		printf("\n%p < %p < %p < %p\n", rml_young_region, RML_UNTAGPTR(rmlA0), RML_UNTAGPTR(rmlA2), rml_young_limit); 
		*/
		/* check if both are in the young generation 
		if(RML_UNTAGPTR(rmlA0) - (void*)(rml_young_region) >= 0 &&
			(void*)(rml_young_limit) - RML_UNTAGPTR(rmlA0) > 0 && 
			RML_UNTAGPTR(rmlA2) - (void*)(rml_young_region) >= 0 &&
			(void*)(rml_young_limit) - RML_UNTAGPTR(rmlA2) > 0)
		{
			rmlA0 = arr;
			RML_TAILCALLK(rmlSC);
		}
		*/
		/* also check here if the array is not alreay in the trail */
		for (idx = rml_array_trail_size; &rml_array_trail[idx] >= rmlATP; idx--)
		if (rml_array_trail[idx] == rmlA0) /* if found, do not add again */
		{
			rmlA0 = arr;
			RML_TAILCALLK(rmlSC);
		}
		/* add the address of the array into the roots to be
		taken into consideration at the garbage collection time */
		if( rmlATP == &rml_array_trail[0] ) 
		{
			(void)fprintf(stderr, "Array Trail Overflow!\n");
			rml_exit(1);
		}
		*--rmlATP = rmlA0;
	}
	rmlA0 = arr;
	RML_TAILCALLK(rmlSC);

#else /* non destructive array implementation (using copy) */

	rml_uint_t nelts = 0;
	void *vec = rmlA0;
	void *data;
	rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
	nelts = RML_HDRSLOTS(RML_GETHDR(vec));
	if( i-1 >= nelts ) 
	{
		RML_TAILCALLK(rmlFC);
	} 
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
	RML_TAILCALLK(rmlSC);

#endif

}
RML_END_LABEL

/* arr-vec.c */
RML_BEGIN_LABEL(RML__array_5fvector)
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
		/* re-read after alloc, it may have been moved */
		vec = rmlA0;
		vec_new->header = RML_STRUCTHDR(nelts, 0);
		rmlA0 = RML_TAGPTR(vec_new);
		/* copy the old vector */
		for(i = 0; i < nelts; i++)
			*vecp++ = RML_STRUCTDATA(vec)[i];
	}
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* arr-copy.c */
RML_BEGIN_LABEL(RML__array_5fcopy)
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
		/* re-read after alloc, it may have been moved */
		vec = rmlA0;
		vec_new->header = RML_STRUCTHDR(nelts, 0);
		rmlA0 = RML_TAGPTR(vec_new);
		/* copy the old vector */
		for(i = 0; i < nelts; i++)
			*vecp++ = RML_STRUCTDATA(vec)[i];
	}
	RML_TAILCALLK(rmlSC);
}
RML_END_LABEL
