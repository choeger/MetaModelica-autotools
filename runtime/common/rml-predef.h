
/*
 * rml-predef.h
 */

extern void rml_prim_motor(rml_labptr_t);
extern int rml_prim_once(rml_labptr_t);	/* C calls RML */
extern RML_FORWARD_LABEL(Main__main);	/* user code's entry point */
/* let the print relation be available for non-debug-version-rml-library also */
extern void rmldb_var_print(void *p);

/*
 * Standard procedures
 * adrpo added Java named RML buildin relations 2005-11-10
 */
extern RML_FORWARD_LABEL(RML__bool_5fand);
extern RML_FORWARD_LABEL(RML__bool_5fnot);
extern RML_FORWARD_LABEL(RML__bool_5for);
   /* java named */
#define RML__boolAnd RML__bool_5fand
#define RML__boolNot RML__bool_5fnot
#define RML__boolOr RML__bool_5for


/* chars */
extern RML_FORWARD_LABEL(RML__int_5fint);
extern RML_FORWARD_LABEL(RML__list_5fstring);
extern RML_FORWARD_LABEL(RML__string_5flist);
extern RML_FORWARD_LABEL(RML__string_5fnth);
extern RML_FORWARD_LABEL(RML__string_5fget);
extern RML_FORWARD_LABEL(RML__string_5fsetnth);
extern RML_FORWARD_LABEL(RML__string_5fupdate);
extern RML_FORWARD_LABEL(RML__string_5fequal);
extern RML_FORWARD_LABEL(RML__string_5fcompare);
#define RML__char_5fint	RML__int_5fint
#define RML__int_5fchar	RML__int_5fint
   /* java named */
#define RML__intChar        RML__int_5fint
#define RML__charInt        RML__int_5fint
#define RML__listString     RML__list_5fstring
#define RML__stringList     RML__string_5flist
#define RML__stringNth      RML__string_5fnth
#define RML__stringGet      RML__string_5fget
#define RML__stringSetNth   RML__string_5fset_5fnth
#define RML__stringUpdate   RML__string_5fupdate
#define RML__stringEqual    RML__string_5fequal
#define RML__stringCompare  RML__string_5fcompare

/* string chars */
extern RML_FORWARD_LABEL(RML__string_5fchar_5fint);
extern RML_FORWARD_LABEL(RML__int_5fstring_5fchar);
extern RML_FORWARD_LABEL(RML__list_5fstring_5fchar);
extern RML_FORWARD_LABEL(RML__string_5flist_5fstring_5fchar);
extern RML_FORWARD_LABEL(RML__string_5fnth_5fstring_5fchar);
extern RML_FORWARD_LABEL(RML__string_5fget_5fstring_5fchar);
extern RML_FORWARD_LABEL(RML__string_5fsetnth_5fstring_5fchar);
extern RML_FORWARD_LABEL(RML__string_5fupdate_5fstring_5fchar);
   /* java named */
#define RML__stringCharInt           RML__string_5fchar_5fint
#define RML__intStringChar           RML__int_5fstring_5fchar
#define RML__listStringChar          RML__list_5fstring_5fchar
#define RML__stringListStringChar    RML__string_5flist_5fstring_5fchar
#define RML__stringNthStringChar     RML__string_5fnth_5fstring_5fchar
#define RML__stringGetStringChar     RML__string_5fget_5fstring_5fchar
#define RML__stringSetNthStringChar  RML__string_5fsetnth_5fstring_5fchar
#define RML__stringUpdateStringChar  RML__string_5fupdate_5fstring_5fchar

extern RML_FORWARD_LABEL(RML__int_5fabs);
extern RML_FORWARD_LABEL(RML__int_5fadd);
extern RML_FORWARD_LABEL(RML__int_5fdiv);
extern RML_FORWARD_LABEL(RML__int_5feq);
extern RML_FORWARD_LABEL(RML__int_5fge);
extern RML_FORWARD_LABEL(RML__int_5fgt);
extern RML_FORWARD_LABEL(RML__int_5fle);
extern RML_FORWARD_LABEL(RML__int_5flt);
extern RML_FORWARD_LABEL(RML__int_5fmax);
extern RML_FORWARD_LABEL(RML__int_5fmin);
extern RML_FORWARD_LABEL(RML__int_5fmod);
extern RML_FORWARD_LABEL(RML__int_5fmul);
extern RML_FORWARD_LABEL(RML__int_5fne);
extern RML_FORWARD_LABEL(RML__int_5fneg);
extern RML_FORWARD_LABEL(RML__int_5freal);
extern RML_FORWARD_LABEL(RML__int_5fstring);
extern RML_FORWARD_LABEL(RML__int_5fsub);
   /* java named */
#define RML__intAbs    RML__int_5fabs
#define RML__intAdd    RML__int_5fadd
#define RML__intDiv    RML__int_5fdiv
#define RML__intEq     RML__int_5feq
#define RML__intGe     RML__int_5fge
#define RML__intGt     RML__int_5fgt
#define RML__intLe     RML__int_5fle
#define RML__intLt     RML__int_5flt
#define RML__intMax    RML__int_5fmax
#define RML__intMin    RML__int_5fmin
#define RML__intMod    RML__int_5fmod
#define RML__intMul    RML__int_5fmul
#define RML__intNe     RML__int_5fne
#define RML__intNeg    RML__int_5fneg
#define RML__intReal   RML__int_5freal
#define RML__intString RML__int_5fstring
#define RML__intSub    RML__int_5fsub

/* lists */
extern RML_FORWARD_LABEL(RML__list_5fappend);
extern RML_FORWARD_LABEL(RML__list_5fdelete);
extern RML_FORWARD_LABEL(RML__list_5flength);
extern RML_FORWARD_LABEL(RML__list_5fmember);
extern RML_FORWARD_LABEL(RML__list_5fnth);
extern RML_FORWARD_LABEL(RML__list_5fget);
extern RML_FORWARD_LABEL(RML__list_5freverse);
extern RML_FORWARD_LABEL(RML__list_5fvector);
extern RML_FORWARD_LABEL(RML__list_5farray);
   /* java named */
#define RML__listAppend  RML__list_5fappend
#define RML__listDelete  RML__list_5fdelete
#define RML__listLength  RML__list_5flength
#define RML__listMember  RML__list_5fmember
#define RML__listNth     RML__list_5fnth
#define RML__listGet     RML__list_5fget
#define RML__listReverse RML__list_5freverse
#define RML__listVector  RML__list_5fvector
#define RML__listArray   RML__list_5farray

/* logical variables */
extern RML_FORWARD_LABEL(RML__lvar_5fget);
extern RML_FORWARD_LABEL(RML__lvar_5fnew);
extern RML_FORWARD_LABEL(RML__lvar_5fset);
   /* java named */
#define RML__lvarGet RML__lvar_5fget
#define RML__lvarNew RML__lvar_5fnew
#define RML__lvarSet RML__lvar_5fset

/* reals */
extern RML_FORWARD_LABEL(RML__real_5fabs);
extern RML_FORWARD_LABEL(RML__real_5fadd);
extern RML_FORWARD_LABEL(RML__real_5fatan);
extern RML_FORWARD_LABEL(RML__real_5fcos);
extern RML_FORWARD_LABEL(RML__real_5fdiv);
extern RML_FORWARD_LABEL(RML__real_5feq);
extern RML_FORWARD_LABEL(RML__real_5fexp);
extern RML_FORWARD_LABEL(RML__real_5ffloor);
extern RML_FORWARD_LABEL(RML__real_5fge);
extern RML_FORWARD_LABEL(RML__real_5fgt);
extern RML_FORWARD_LABEL(RML__real_5fint);
extern RML_FORWARD_LABEL(RML__real_5fle);
extern RML_FORWARD_LABEL(RML__real_5fln);
extern RML_FORWARD_LABEL(RML__real_5flt);
extern RML_FORWARD_LABEL(RML__real_5fmax);
extern RML_FORWARD_LABEL(RML__real_5fmin);
extern RML_FORWARD_LABEL(RML__real_5fmod);
extern RML_FORWARD_LABEL(RML__real_5fmul);
extern RML_FORWARD_LABEL(RML__real_5fne);
extern RML_FORWARD_LABEL(RML__real_5fneg);
extern RML_FORWARD_LABEL(RML__real_5fpow);
extern RML_FORWARD_LABEL(RML__real_5fsin);
extern RML_FORWARD_LABEL(RML__real_5fsqrt);
extern RML_FORWARD_LABEL(RML__real_5fstring);
extern RML_FORWARD_LABEL(RML__real_5fsub);
extern RML_FORWARD_LABEL(RML__real_5fasin);
extern RML_FORWARD_LABEL(RML__real_5facos);
extern RML_FORWARD_LABEL(RML__real_5fatan);
extern RML_FORWARD_LABEL(RML__real_5fatan2);
extern RML_FORWARD_LABEL(RML__real_5fcosh);
extern RML_FORWARD_LABEL(RML__real_5flog);
extern RML_FORWARD_LABEL(RML__real_5flog10);
extern RML_FORWARD_LABEL(RML__real_5fsinh);
extern RML_FORWARD_LABEL(RML__real_5ftanh);
   /* java named */
#define RML__realAbs    RML__real_5fabs
#define RML__realAdd    RML__real_5fadd
#define RML__realAtan   RML__real_5fatan
#define RML__realCos    RML__real_5fcos
#define RML__realDiv    RML__real_5fdiv
#define RML__realEq     RML__real_5feq
#define RML__realExp    RML__real_5fexp
#define RML__realFloor  RML__real_5ffloor
#define RML__realGe     RML__real_5fge
#define RML__realGt     RML__real_5fgt
#define RML__realInt    RML__real_5fint
#define RML__realLe     RML__real_5fle
#define RML__realLn     RML__real_5fln
#define RML__realLt     RML__real_5flt
#define RML__realMax    RML__real_5fmax
#define RML__realMin    RML__real_5fmin
#define RML__realMod    RML__real_5fmod
#define RML__realMul    RML__real_5fmul
#define RML__realNe     RML__real_5fne
#define RML__realNeg    RML__real_5fneg
#define RML__realPow    RML__real_5fpow
#define RML__realSin    RML__real_5fsin
#define RML__realSqrt   RML__real_5fsqrt
#define RML__realString RML__real_5fstring
#define RML__realSub    RML__real_5fsub
#define RML__realAsin   RML__real_5fasin
#define RML__realAcos   RML__real_5facos
#define RML__realAtan2  RML__real_5fatan2
#define RML__realCosh   RML__real_5fcosh
#define RML__realLog    RML__real_5flog
#define RML__realLog10  RML__real_5flog10
#define RML__realSinh   RML__real_5fsinh
#define RML__realTanh   RML__real_5ftanh

/* strings */
extern RML_FORWARD_LABEL(RML__string_5fappend);
extern RML_FORWARD_LABEL(RML__string_5fint);
extern RML_FORWARD_LABEL(RML__string_5flength);
extern RML_FORWARD_LABEL(RML__string_5fappend_5flist);
/* java named */
#define RML__stringAppend RML__string_5fappend
#define RML__stringInt    RML__string_5fint
#define RML__stringLength RML__string_5flength
#define RML__stringAppendList RML__string_5fappend_5flist

/* declarative vectors */
extern RML_FORWARD_LABEL(RML__vector_5flength);
extern RML_FORWARD_LABEL(RML__vector_5flist);
extern RML_FORWARD_LABEL(RML__vector_5fnth);
extern RML_FORWARD_LABEL(RML__vector_5fget);
extern RML_FORWARD_LABEL(RML__vector_5fsetnth);
extern RML_FORWARD_LABEL(RML__vector_5fupdate);
extern RML_FORWARD_LABEL(RML__vector_5fcreate);
extern RML_FORWARD_LABEL(RML__vector_5fadd);
extern RML_FORWARD_LABEL(RML__vector_5farray);
extern RML_FORWARD_LABEL(RML__vector_5fcopy);
   /* java named */
#define RML__vectorLength RML__vector_5flength
#define RML__vectorList   RML__vector_5flist
#define RML__vectorNth    RML__vector_5fnth
#define RML__vectorGet    RML__vector_5fget
#define RML__vectorSetNth RML__vector_5fsetnth
#define RML__vectorUpdate RML__vector_5fupdate
#define RML__vectorCreate RML__vector_5fcreate
#define RML__vectorAdd    RML__vector_5fadd
#define RML__vectorArray  RML__vector_5farray
#define RML__vectorCopy   RML__vector_5fcopy

/* mutable arrays */
extern RML_FORWARD_LABEL(RML__array_5flength);
extern RML_FORWARD_LABEL(RML__array_5flist);
extern RML_FORWARD_LABEL(RML__array_5fnth);
extern RML_FORWARD_LABEL(RML__array_5fget);
extern RML_FORWARD_LABEL(RML__array_5fsetnth);
extern RML_FORWARD_LABEL(RML__array_5fupdate);
extern RML_FORWARD_LABEL(RML__array_5fcreate);
extern RML_FORWARD_LABEL(RML__array_5fadd);
extern RML_FORWARD_LABEL(RML__array_5fvector);
extern RML_FORWARD_LABEL(RML__array_5fcopy);
   /* java named */
#define RML__arrayLength RML__array_5flength
#define RML__arrayList   RML__array_5flist
#define RML__arrayNth    RML__array_5fnth
#define RML__arrayGet    RML__array_5fget
#define RML__arraySetNth RML__array_5fsetnth
#define RML__arrayUpdate RML__array_5fupdate
#define RML__arrayCreate RML__array_5fcreate
#define RML__arrayAdd    RML__array_5fadd
#define RML__arrayVector RML__array_5fvector
#define RML__arrayCopy   RML__array_5fcopy

/* if expressions */
extern RML_FORWARD_LABEL(RML__if_5fexp);
#define RML__ifExp   RML__if_5fexp

/* misc */
extern RML_FORWARD_LABEL(RML__clock);
extern RML_FORWARD_LABEL(RML__print);
extern RML_FORWARD_LABEL(RML__tick);

/* debugging */
/* let these relation be available in all rml libraries */
/* however their code depends on _RMLDB_DEFINED_ */
extern RML_FORWARD_LABEL(RML__debug_5fprint);
extern RML_FORWARD_LABEL(RML__debug);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin01);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin02);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin03);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin04);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin05);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin06);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin07);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin08);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin09);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin10);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin11);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin12);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin13);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin14);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin15);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fin16);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout01);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout02);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout03);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout04);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout05);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout06);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout07);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout08);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout09);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout10);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout11);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout12);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout13);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout14);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout15);
extern RML_FORWARD_LABEL(RML__debug_5fpush_5fout16);
extern RML_FORWARD_LABEL(RML__debug_5fshow_5fdepth);

extern void RML__call_debug(char* fileName, int sp, int ep, int sl, int sc, int el, int ec, char* relation, char* call);
