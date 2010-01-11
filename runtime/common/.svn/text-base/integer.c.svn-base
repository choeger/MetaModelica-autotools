#include "rml.h"
#include <stdio.h>
#include <string.h>

/* int_abs.c */
RML_BEGIN_LABEL(RML__int_5fabs)
{
    rmlA0 = RML_PRIM_INT_ABS(rmlA0);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_add.c */
RML_BEGIN_LABEL(RML__int_5fadd)
{
    rmlA0 = RML_PRIM_INT_ADD(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_div.c */
RML_BEGIN_LABEL(RML__int_5fdiv)
{
    if( rmlA1 != RML_IMMEDIATE(RML_TAGFIXNUM(0)) ) {
	rmlA0 = RML_PRIM_INT_DIV(rmlA0, rmlA1);
	RML_TAILCALLK(rmlSC);
    } else
	RML_TAILCALLK(rmlFC);
}
RML_END_LABEL

/* int_eq.c */
RML_BEGIN_LABEL(RML__int_5feq)
{
    rmlA0 = RML_PRIM_INT_EQ(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_ge.c */
RML_BEGIN_LABEL(RML__int_5fge)
{
    rmlA0 = RML_PRIM_INT_GE(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_gt.c */
RML_BEGIN_LABEL(RML__int_5fgt)
{
    rmlA0 = RML_PRIM_INT_GT(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_int.c (implements both char_int and int_char) */
RML_BEGIN_LABEL(RML__int_5fint)
{
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* 
 * adrpo added int_string_char and string_char_int
 */
RML_BEGIN_LABEL(RML__int_5fstring_5fchar)
{
	struct rml_string *strnew = rml_prim_mkstring(1, 1);
	strnew->data[0] = (rml_uint_t)RML_UNTAGFIXNUM(rmlA0);
	strnew->data[1] = '\0';
	rmlA0 = RML_TAGPTR(strnew);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__string_5fchar_5fint)
{
	rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM((rml_uint_t)RML_STRINGDATA(rmlA0)[0]));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* int_le.c */
RML_BEGIN_LABEL(RML__int_5fle)
{
    rmlA0 = RML_PRIM_INT_LE(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* int_lt.c */
RML_BEGIN_LABEL(RML__int_5flt)
{
    rmlA0 = RML_PRIM_INT_LT(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_max.c */
RML_BEGIN_LABEL(RML__int_5fmax)
{
    rmlA0 = RML_PRIM_INT_MAX(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_min.c */
RML_BEGIN_LABEL(RML__int_5fmin)
{
    rmlA0 = RML_PRIM_INT_MIN(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_mod.c */
RML_BEGIN_LABEL(RML__int_5fmod)
{
    if( rmlA1 != RML_IMMEDIATE(RML_TAGFIXNUM(0)) ) {
	rmlA0 = RML_PRIM_INT_MOD(rmlA0, rmlA1);
	RML_TAILCALLK(rmlSC);
    } else
	RML_TAILCALLK(rmlFC);
}
RML_END_LABEL

/* int_mul.c */
RML_BEGIN_LABEL(RML__int_5fmul)
{
    rmlA0 = RML_PRIM_INT_MUL(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_ne.c */
RML_BEGIN_LABEL(RML__int_5fne)
{
    rmlA0 = RML_PRIM_INT_NE(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_neg.c */
RML_BEGIN_LABEL(RML__int_5fneg)
{
    rmlA0 = RML_PRIM_INT_NEG(rmlA0);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_real.c */
RML_BEGIN_LABEL(RML__int_5freal)
{
    rmlA0 = rml_prim_mkreal((double)RML_UNTAGFIXNUM(rmlA0));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_str.c */
RML_BEGIN_LABEL(RML__int_5fstring)
{
    char buf[32];
    struct rml_string *str;

    (void)sprintf(buf, "%ld", (long)RML_UNTAGFIXNUM(rmlA0));
    str = rml_prim_mkstring(strlen(buf), 0);
    (void)strcpy(str->data, buf);	/* this also sets the ending '\0' */
    rmlA0 = RML_TAGPTR(str);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* int_sub.c */
RML_BEGIN_LABEL(RML__int_5fsub)
{
    rmlA0 = RML_PRIM_INT_SUB(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL
