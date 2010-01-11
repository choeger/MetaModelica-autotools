#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "rml.h"

/* p_mkreal.c */
void *rml_prim_mkreal(double d)
{
    rml_uint_t nwords = RML_SIZE_DBL/RML_SIZE_INT + 1;
    struct rml_real *p = (struct rml_real*)rml_prim_alloc(nwords, 0);
    rml_prim_set_real(p, d);
    p->header = RML_REALHDR;
    return RML_TAGPTR(p);
}

/* p_get_real.c */
#ifdef	RML_DBL_STRICT
double rml_prim_get_real(void *p)
{
    union rml_double_as_words u;
    u.data[0] = RML_REALDATA(p)[0];
    u.data[1] = RML_REALDATA(p)[1];
    return u.d;
}
#endif	/* RML_DBL_STRICT */

/* p_set_real.c */
#ifdef	RML_DBL_STRICT
void rml_prim_set_real(struct rml_real *p, double d)
{
    union rml_double_as_words u;
    u.d = d;
    p->data[0] = u.data[0];
    p->data[1] = u.data[1];
}
#endif	/* RML_DBL_STRICT */

/* real_abs.c */
RML_BEGIN_LABEL(RML__real_5fabs)
{
    double d = rml_prim_get_real(rmlA0);
    if( d < 0.0 )
	rmlA0 = rml_prim_mkreal(-d);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_add.c */
RML_BEGIN_LABEL(RML__real_5fadd)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = rml_prim_mkreal(d0 + d1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_atan.c */
RML_BEGIN_LABEL(RML__real_5fatan)
{
    rmlA0 = rml_prim_mkreal(atan(rml_prim_get_real(rmlA0)));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_cos.c */
RML_BEGIN_LABEL(RML__real_5fcos)
{
    rmlA0 = rml_prim_mkreal(cos(rml_prim_get_real(rmlA0)));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_div.c */
RML_BEGIN_LABEL(RML__real_5fdiv)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    if( d1 == 0.0 )
	RML_TAILCALLK(rmlFC);
    rmlA0 = rml_prim_mkreal(d0 / d1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_eq.c */
RML_BEGIN_LABEL(RML__real_5feq)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = (d0 == d1) ? RML_TRUE : RML_FALSE;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_exp.c */
RML_BEGIN_LABEL(RML__real_5fexp)
{
    rmlA0 = rml_prim_mkreal(exp(rml_prim_get_real(rmlA0)));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_floor.c */
RML_BEGIN_LABEL(RML__real_5ffloor)
{
    rmlA0 = rml_prim_mkreal(floor(rml_prim_get_real(rmlA0)));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_ge.c */
RML_BEGIN_LABEL(RML__real_5fge)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = (d0 >= d1) ? RML_TRUE : RML_FALSE;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_gt.c */
RML_BEGIN_LABEL(RML__real_5fgt)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = (d0 > d1) ? RML_TRUE : RML_FALSE;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_int.c */
RML_BEGIN_LABEL(RML__real_5fint)
{
    double d0 = floor(rml_prim_get_real(rmlA0));
    rml_sint_t i0 = RML_TAGFIXNUM((rml_sint_t)d0);
    if( (double)RML_UNTAGFIXNUM(i0) == d0 ) {
	rmlA0 = RML_IMMEDIATE(i0);
	RML_TAILCALLK(rmlSC);
    } else
	RML_TAILCALLK(rmlFC);
}
RML_END_LABEL

/* real_le.c */
RML_BEGIN_LABEL(RML__real_5fle)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = (d0 <= d1) ? RML_TRUE : RML_FALSE;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_ln.c */
RML_BEGIN_LABEL(RML__real_5fln)
{
    double d0 = rml_prim_get_real(rmlA0);
    if( d0 > 0.0 ) {
	rmlA0 = rml_prim_mkreal(log(d0));
	RML_TAILCALLK(rmlSC);
    } else
	RML_TAILCALLK(rmlFC);
}
RML_END_LABEL

/* real_lt.c */
RML_BEGIN_LABEL(RML__real_5flt)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = (d0 < d1) ? RML_TRUE : RML_FALSE;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_max.c */
RML_BEGIN_LABEL(RML__real_5fmax)
{
    if( rml_prim_get_real(rmlA1) > rml_prim_get_real(rmlA0) )
	rmlA0 = rmlA1;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_min.c */
RML_BEGIN_LABEL(RML__real_5fmin)
{
    if( rml_prim_get_real(rmlA1) < rml_prim_get_real(rmlA0) )
	rmlA0 = rmlA1;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_mod.c */
RML_BEGIN_LABEL(RML__real_5fmod)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    if( d1 == 0.0 )
	RML_TAILCALLK(rmlFC);
    rmlA0 = rml_prim_mkreal(fmod(d0, d1));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_mul.c */
RML_BEGIN_LABEL(RML__real_5fmul)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = rml_prim_mkreal(d0 * d1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_ne.c */
RML_BEGIN_LABEL(RML__real_5fne)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = (d0 != d1) ? RML_TRUE : RML_FALSE;
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_neg.c */
RML_BEGIN_LABEL(RML__real_5fneg)
{
    rmlA0 = rml_prim_mkreal(-rml_prim_get_real(rmlA0));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_pow.c */
RML_BEGIN_LABEL(RML__real_5fpow)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = rml_prim_mkreal(pow(d0, d1));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_sin.c */
RML_BEGIN_LABEL(RML__real_5fsin)
{
    rmlA0 = rml_prim_mkreal(sin(rml_prim_get_real(rmlA0)));
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* real_sqrt.c */
RML_BEGIN_LABEL(RML__real_5fsqrt)
{
    double d0 = rml_prim_get_real(rmlA0);
    if( d0 >= 0.0 ) {
	rmlA0 = rml_prim_mkreal(sqrt(d0));
	RML_TAILCALLK(rmlSC);
    } else
	RML_TAILCALLK(rmlFC);
}
RML_END_LABEL

/* real_str.c */
RML_BEGIN_LABEL(RML__real_5fstring)
{
#if defined(__MINGW32__) || defined(_MSC_VER)
	int expo;
	int count;
	int i;
	int frac;
    char buf[32], *q;
    struct rml_string *str;

    /* The reason for this code is that no printf f.p. conversion
     * modifier does exactly what we want:
     * %f doesn't generate exponents for large-magnitude numbers
     * %e generates exponents even for small-magnitude numbers
     * %g does use exponents when necessary, but also omits zero fractions
     * %#g emits zero fractions, but also emits excessive trailing zeros
     *
     * As a workaround, use %g but scan the output and append ".0" if no
     * fraction or exponent was emitted.
     */
    sprintf(buf, "%.15g", rml_prim_get_real(rmlA0));
	expo = 0;
	count = 0;
	frac = 0;
	for(q = buf; ;) {	/* make sure it doesn't look like an int */
	char c = *q++;
	if( isdigit(c) ) {
		if (expo) count++;
	    continue;
	}

	if( c == '\0' && ! expo && !frac) {	/* looks like int -- append ".0" */
	    q[-1] = '.';
	    q[0] = '0';
	    q[1] = '\0';
	    break;
	}
	else if (c == '\0')
	{
		/* This makes sure that the 1.0e-/+005 is rewritten to 1.0e-/+05 like in 
		the cygwin version so that the testsuite works */ 
		/* printf("buf:%s, q:%s, expo:%d, count:%d\n", buf, q, expo, count); */
		if (expo && count >= 3 && q[-1-count] == '0') {
			for(i=count; i>0; i--) {
				q[-1-i] = q[-i];
			}
		}
		break;
	}

	if( c == '-' || c == '+')
	    continue;

	if( c == 'e' ) {
		expo = 1;
		continue;
	}

	if (c == '.') frac = 1;
    }

    str = rml_prim_mkstring(strlen(buf), 0);
    strcpy(str->data, buf);	/* this also sets the ending '\0' */
    rmlA0 = RML_TAGPTR(str);
    RML_TAILCALLK(rmlSC);

#else /* Linux or other stuff */

    char buf[32], *q;
    struct rml_string *str;

    /* The reason for this code is that no printf f.p. conversion
     * modifier does exactly what we want:
     * %f doesn't generate exponents for large-magnitude numbers
     * %e generates exponents even for small-magnitude numbers
     * %g does use exponents when necessary, but also omits zero fractions
     * %#g emits zero fractions, but also emits excessive trailing zeros
     *
     * As a workaround, use %g but scan the output and append ".0" if no
     * fraction or exponent was emitted.
     */
    sprintf(buf, "%.15g", rml_prim_get_real(rmlA0));
    for(q = buf; ;) {	/* make sure it doesn't look like an int */
	char c = *q++;
	if( isdigit(c) )
	    continue;
	if( c == '\0' ) {	/* looks like int -- append ".0" */
	    q[-1] = '.';
	    q[0] = '0';
	    q[1] = '\0';
	    break;
	}
	if( c == '-' )
	    continue;
	/* If we get here we found
	 * '.', indicating a fraction (ok),
	 * 'e', indicating an exponent (ok),
	 * or something else, probably indicating nan or inf (bad).
	 * In either case, leave the string as-is.
	 */
	break;
    }
    str = rml_prim_mkstring(strlen(buf), 0);
    strcpy(str->data, buf);	/* this also sets the ending '\0' */
    rmlA0 = RML_TAGPTR(str);
    RML_TAILCALLK(rmlSC);
#endif
}
RML_END_LABEL

/* real_sub.c */
RML_BEGIN_LABEL(RML__real_5fsub)
{
    double d0 = rml_prim_get_real(rmlA0);
    double d1 = rml_prim_get_real(rmlA1);
    rmlA0 = rml_prim_mkreal(d0 - d1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5fasin)
{
  rmlA0 = rml_prim_mkreal(asin(rml_prim_get_real(rmlA0)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5facos)
{
  rmlA0 = rml_prim_mkreal(acos(rml_prim_get_real(rmlA0)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5fatan2)
{
  rmlA0 = rml_prim_mkreal(atan2(rml_prim_get_real(rmlA0),rml_prim_get_real(rmlA1)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5fcosh)
{
  rmlA0 = rml_prim_mkreal(cosh(rml_prim_get_real(rmlA0)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5flog)
{
  rmlA0 = rml_prim_mkreal(log(rml_prim_get_real(rmlA0)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5flog10)
{
  rmlA0 = rml_prim_mkreal(log10(rml_prim_get_real(rmlA0)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5fsinh)
{
  rmlA0 = rml_prim_mkreal(sinh(rml_prim_get_real(rmlA0)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__real_5ftanh)
{
  rmlA0 = rml_prim_mkreal(tanh(rml_prim_get_real(rmlA0)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL
