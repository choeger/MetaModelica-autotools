#include "rml.h"

/* list_append.c */
RML_BEGIN_LABEL(RML__list_5fappend)
{
    rml_uint_t nelts;
    void *lst;

    /* count the number of elements in the first list */
    lst = rmlA0;
    nelts = 0;
    while( RML_GETHDR(lst) == RML_CONSHDR ) {
	lst = RML_CDR(lst);
	++nelts;
    }

    /* cons up fresh copy of first list, tack on second list last */
    if( nelts == 0 )
	rmlA0 = rmlA1;
    else {
	void **chunk = (void**)rml_prim_alloc(3*nelts, 2);
	lst = rmlA0;
	rmlA0 = RML_TAGPTR(chunk);
	do {
	    chunk[0] = RML_IMMEDIATE(RML_CONSHDR);
	    chunk[1] = RML_CAR(lst);
	    chunk[2] = RML_TAGPTR(chunk + 3);
	    lst = RML_CDR(lst);
	    chunk += 3;
	} while( --nelts != 0 );
	/* set the CDR of the last copied CONS to the second list */
	chunk[-1] = rmlA1;
    }

    /* return resulting list */
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* list-arr.c */
RML_BEGIN_LABEL(RML__list_5farray)
{
    rml_uint_t nelts = 0;

    /* first compute the length of the list */
    {
	void *lst = rmlA0;
	for(; RML_GETHDR(lst) == RML_CONSHDR; ++nelts, lst = RML_CDR(lst))
	    ;
    }
    /* then allocate and initialize the vector */
    {
	struct rml_struct *vec = (struct rml_struct*)rml_prim_alloc(1+nelts, 1);
	void *lst = rmlA0;
	void **vecp = vec->data;
	vec->header = RML_STRUCTHDR(nelts, 0);
	rmlA0 = RML_TAGPTR(vec);
	for(; nelts > 0; --nelts, lst = RML_CDR(lst))
	    *vecp++ = RML_CAR(lst);
    }
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* list-delete.c */
RML_BEGIN_LABEL(RML__list_5fdelete)
{
    rml_sint_t nelts = RML_UNTAGFIXNUM(rmlA1);
    if( nelts < 0 )
	RML_TAILCALLK(rmlFC);
    else if( nelts == 0 ) {
	if( RML_GETHDR(rmlA0) == RML_CONSHDR )
	    rmlA0 = RML_CDR(rmlA0);
	else
	    RML_TAILCALLK(rmlFC);
    } else { /* nelts > 0 */
	void **chunk = (void**)rml_prim_alloc(3*nelts, 1);
	void *lst = rmlA0;
	rmlA0 = RML_TAGPTR(chunk);
	for(;;) {
	    if( RML_GETHDR(lst) == RML_CONSHDR ) {
		if( nelts == 0 ) {
		    chunk[-1] = RML_CDR(lst);
		    break;
		} else {
		    chunk[0] = RML_IMMEDIATE(RML_CONSHDR);
		    chunk[1] = RML_CAR(lst);
		    chunk[2] = RML_TAGPTR(chunk + 3);
		    lst = RML_CDR(lst);
		    chunk += 3;
		    --nelts;
		    continue;
		}
	    } else	/* NIL */
		RML_TAILCALLK(rmlFC);
	}
    }

    /* return resulting list */
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* list_length.c */
RML_BEGIN_LABEL(RML__list_5flength)
{
    void *lst = rmlA0;
    rml_uint_t len = 0;
    for(; RML_GETHDR(lst) == RML_CONSHDR; ++len, lst = RML_CDR(lst))
	;
    rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(len));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* list-member.c */
RML_BEGIN_LABEL(RML__list_5fmember)
{
    void *x = rmlA0;
    void *ys = rmlA1;
    void *result = RML_FALSE;
    for(; RML_GETHDR(ys) == RML_CONSHDR; ys = RML_CDR(ys)) {
	if( rml_prim_equal(x, RML_CAR(ys)) != RML_FALSE ) {
	    result = RML_TRUE;
	    break;
	}
    }
    rmlA0 = result;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* list-nth.c */
RML_BEGIN_LABEL(RML__list_5fnth)
{
    rml_sint_t i = RML_UNTAGFIXNUM(rmlA1);
    void *lst = rmlA0;
    for(; RML_GETHDR(lst) == RML_CONSHDR; --i, lst = RML_CDR(lst)) {
	if( i == 0 ) {
	    rmlA0 = RML_CAR(lst);
	    RML_TAILCALLK(rmlSC);
	}
    }
    RML_TAILCALLK(rmlFC);
}
RML_END_LABEL

/* list-get.c */
RML_BEGIN_LABEL(RML__list_5fget)
{
    rml_sint_t i = RML_UNTAGFIXNUM(rmlA1);
    void *lst = rmlA0;
	i--; /* list_get starts the index at 1 */
	if (i < 0) RML_TAILCALLK(rmlFC);
    for(; RML_GETHDR(lst) == RML_CONSHDR; --i, lst = RML_CDR(lst)) 
	{
		if ( i == 0 ) 
		{
			rmlA0 = RML_CAR(lst);
			RML_TAILCALLK(rmlSC);
		}
    }
    RML_TAILCALLK(rmlFC);
}
RML_END_LABEL

/* list_reverse.c */
RML_BEGIN_LABEL(RML__list_5freverse)
{
    void *a1;		/* cached A1 */
    struct rml_struct *cons;

    /* A1 := A0; A0 := NIL */
    a1 = rmlA0;
    rmlA0 = RML_TAGPTR(&rml_prim_nil);

    /* while CONSP(A1) do A0 := CONS(CAR(A1), A0); A1 := CDR(A1) end */
    while( RML_GETHDR(a1) == RML_CONSHDR ) {
	rmlA1 = a1;
	cons = (struct rml_struct*)rml_prim_alloc(3, 2);
	a1 = rmlA1;
	cons->header = RML_CONSHDR;
	cons->data[0] = RML_CAR(a1);
	cons->data[1] = rmlA0;
	rmlA0 = RML_TAGPTR(cons);
	a1 = RML_CDR(a1);
    }

    /* return A0 */
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* list-str.c */
RML_BEGIN_LABEL(RML__list_5fstring)
{
    rml_uint_t len = 0;

    /* first compute the length of the list */
    {
	void *lst = rmlA0;
	for(; RML_GETHDR(lst) == RML_CONSHDR; ++len, lst = RML_CDR(lst))
	    ;
    }
    /* then allocate and initialize the string */
    {
	struct rml_string *str = rml_prim_mkstring(len, 1);	/* gets len+1 bytes */
	void *lst = rmlA0;
	unsigned char *s = (unsigned char*)str->data;
	rmlA0 = RML_TAGPTR(str);
	for(; len > 0; --len, lst = RML_CDR(lst))
	    *s++ = RML_UNTAGFIXNUM(RML_CAR(lst));
	*s = '\0';
    }
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* adrpo added string char list to string */
RML_BEGIN_LABEL(RML__string_5fchar_5flist_5fstring)
{
    rml_uint_t len = 0;

    /* first compute the length of the list */
    {
	void *lst = rmlA0;
	for(; RML_GETHDR(lst) == RML_CONSHDR; ++len, lst = RML_CDR(lst))
	    ;
    }
    /* then allocate and initialize the string */
    {
	struct rml_string *str = rml_prim_mkstring(len, 1);	/* gets len+1 bytes */
	void *lst = rmlA0;
	unsigned char *s = (unsigned char*)str->data;
	rmlA0 = RML_TAGPTR(str);
	for(; len > 0; --len, lst = RML_CDR(lst))
	{
		/* printf ("%c ",RML_STRINGDATA(RML_CAR(lst))[0]) */
	    *s++ = RML_STRINGDATA(RML_CAR(lst))[0];
	}
	*s = '\0';
    }
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* list_vec.c */
RML_BEGIN_LABEL(RML__list_5fvector)
{
    rml_uint_t nelts = 0;

    /* first compute the length of the list */
    {
	void *lst = rmlA0;
	for(; RML_GETHDR(lst) == RML_CONSHDR; ++nelts, lst = RML_CDR(lst))
	    ;
    }
    /* then allocate and initialize the vector */
    {
	struct rml_struct *vec = (struct rml_struct*)rml_prim_alloc(1+nelts, 1);
	void *lst = rmlA0;
	void **vecp = vec->data;
	vec->header = RML_STRUCTHDR(nelts, 0);
	rmlA0 = RML_TAGPTR(vec);
	for(; nelts > 0; --nelts, lst = RML_CDR(lst))
	    *vecp++ = RML_CAR(lst);
    }
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


RML_BEGIN_LABEL(RML__list_5fmap)
{
    rml_uint_t nelts;
    void *lst = rmlA0;
    void *function = rmlA1;

    /* count the number of elements in the first list */
    nelts = 0;
    while( RML_GETHDR(lst) == RML_CONSHDR ) {
      lst = RML_CDR(lst);
      ++nelts;
    }
    /* call the relation, to build the second list  */
    if( nelts == 0 )
    { /* do nothing, return nil */ }
    else 
    {
        void **chunk = (void**)rml_prim_alloc(3*nelts, 2);
        lst = rmlA0;
        rmlA0 = RML_TAGPTR(chunk);
        do {
            chunk[0] = RML_IMMEDIATE(RML_CONSHDR);
            rmlA0 = RML_CAR(lst); /* element */;
            RML_TAILCALL(rmlA1 /* fn */,1);
            chunk[1] = rmlA0;
            chunk[2] = RML_TAGPTR(chunk + 3);
            lst = RML_CDR(lst);
            chunk += 3;
        } while( --nelts != 0 );
    }

    /* return resulting list */
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL
