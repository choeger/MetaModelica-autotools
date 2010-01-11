#include "rml.h"

/* bool_and.c */
RML_BEGIN_LABEL(RML__bool_5fand)
{
    rmlA0 = RML_PRIM_BOOL_AND(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* bool_not.c */
RML_BEGIN_LABEL(RML__bool_5fnot)
{
    rmlA0 = RML_PRIM_BOOL_NOT(rmlA0);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* bool_or.c */
RML_BEGIN_LABEL(RML__bool_5for)
{
    rmlA0 = RML_PRIM_BOOL_OR(rmlA0, rmlA1);
    RML_TAILCALLK(rmlSC);
}
RML_END_LABEL
