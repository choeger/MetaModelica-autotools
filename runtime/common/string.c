#include <string.h>
#include "rml.h"

/* p-atoi.c */
rml_sint_t rml_prim_atoi(const struct rml_string *str)
{
  rml_sint_t nchars;
  const unsigned char *s;
  unsigned char c;
  rml_uint_t value;
  int negate;

  nchars = RML_HDRSTRLEN(str->header);
  if( nchars <= 0 )
    return 0;
  s = (const unsigned char*)str->data;
  if( *s == '-' )
    ++s, --nchars, negate = 1;
  else
    negate = 0;
  for(value = 0; --nchars >= 0 && (c = *s++) >= '0' && c <= '9';)
    value = value * 10 + (c - '0');
  if( negate )
    value = -value;
  return (rml_sint_t)value;
}


/* p_mkstring.c */
struct rml_string *rml_prim_mkstring(rml_uint_t nbytes, rml_uint_t nliveargs)
{
  rml_uint_t header = RML_STRINGHDR(nbytes);
  rml_uint_t nwords = RML_HDRSLOTS(header) + 1;
  struct rml_string *p = (struct rml_string*)rml_prim_alloc(nwords, nliveargs);
  p->header = header;
  return p;
}

/* p_stringeq.c */
rml_sint_t rml_prim_stringeq(void *p, rml_uint_t qhdr, const char *q)
{
  return RML_GETHDR(p) == qhdr && !memcmp(RML_STRINGDATA(p), q, RML_HDRSTRLEN(qhdr));
}

/* str_append.c */
RML_BEGIN_LABEL(RML__string_5fappend)
{
  rml_uint_t len0 = RML_HDRSTRLEN(RML_GETHDR(rmlA0));
  rml_uint_t len1 = RML_HDRSTRLEN(RML_GETHDR(rmlA1));
  struct rml_string *str = rml_prim_mkstring(len0 + len1, 2);
  (void)memcpy(&str->data[0], RML_STRINGDATA(rmlA0), len0);
  (void)memcpy(&str->data[len0], RML_STRINGDATA(rmlA1), len1+1);	/* +1 to copy terminating '\0' */
  rmlA0 = RML_TAGPTR(str);
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* str_append_list.c */
RML_BEGIN_LABEL(RML__string_5fappend_5flist)
{
  /* count the length of elements in the first list */
  rml_uint_t len_car = 0;
  rml_uint_t len_cur = 0;
  rml_uint_t len = 0;
  void *lst = rmlA0;
  while( RML_GETHDR(lst) == RML_CONSHDR ) {
    len += RML_HDRSTRLEN(RML_GETHDR(RML_CAR(lst)));
	  lst = RML_CDR(lst);
  }
  struct rml_string *str = rml_prim_mkstring(len, 1);
  /* re-read the rmlA0 as it might have been moved by the GC */
  lst = rmlA0;
  while( RML_GETHDR(lst) == RML_CONSHDR ) {
    void* car = RML_CAR(lst);
    len_car = RML_HDRSTRLEN(RML_GETHDR(car));
    (void)memcpy(
      &str->data[len_cur], 
      RML_STRINGDATA(car), 
      len_car);
    len_cur += len_car;
	  lst = RML_CDR(lst);
  }
  str->data[len_cur+1] = '\0';
  rmlA0 = RML_TAGPTR(str);
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


/* str-int.c */
RML_BEGIN_LABEL(RML__string_5fint)
{
  const struct rml_string *str = (const struct rml_string*)RML_UNTAGPTR(rmlA0);
  rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(rml_prim_atoi(str)));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* str_length.c */
RML_BEGIN_LABEL(RML__string_5flength)
{
  rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(RML_HDRSTRLEN(RML_GETHDR(rmlA0))));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* str_list.c */
RML_BEGIN_LABEL(RML__string_5flist)
{
  /* Allocate a big blob for all the conses, i.e. 3 * #conses words,
  * and then initialize it.
  */
  rml_uint_t nelts = RML_HDRSTRLEN(RML_GETHDR(rmlA0));
  void **consp = (void**)rml_prim_alloc(3*nelts, 1);
  unsigned char *s = (unsigned char*)RML_STRINGDATA(rmlA0) + nelts;
  void *a0 = RML_TAGPTR(&rml_prim_nil);
  /* XXX: we should build the list in address order */
  for(; nelts > 0; a0 = RML_TAGPTR(consp), consp += 3, --nelts) {
    consp[0] = RML_IMMEDIATE(RML_CONSHDR);
    consp[1] = RML_IMMEDIATE(RML_TAGFIXNUM((rml_uint_t)*--s));
    consp[2] = a0;
  }
  rmlA0 = a0;
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* adrpo added string to string char list */
RML_BEGIN_LABEL(RML__string_5flist_5fstring_5fchar)
{
  /* Allocate a big blob for all the conses, i.e. 3+strnwords * #conses words,
  * and then initialize it.
  */
  rml_uint_t nelts = RML_HDRSTRLEN(RML_GETHDR(rmlA0));
  void *a0 = RML_TAGPTR(&rml_prim_nil);
  rml_uint_t strheader = RML_STRINGHDR(1);
  rml_uint_t strnwords = RML_HDRSLOTS(strheader)+1;
  struct rml_string *p;
  void **consp = (void**)rml_prim_alloc((3+strnwords)*nelts, 1);
  void **strStartAddr = consp+(3*nelts); /* where the list ends the strings start */
  unsigned char *s = (unsigned char*)RML_STRINGDATA(rmlA0) + nelts;
  /* XXX: we should build the list in address order */
  for(; nelts > 0; a0 = RML_TAGPTR(consp), consp += 3, strStartAddr +=2, --nelts) 
  {
    consp[0] = RML_IMMEDIATE(RML_CONSHDR);
    p = (struct rml_string *)strStartAddr;
    p->header = strheader;
    p->data[0] = (rml_uint_t)*--s;
    p->data[1] = '\0';
    consp[1] = RML_TAGPTR(p);
    consp[2] = a0;
  }
  rmlA0 = a0;
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* str_nth.c */
RML_BEGIN_LABEL(RML__string_5fnth)
{
  void *str = rmlA0;
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
  if( i >= RML_HDRSTRLEN(RML_GETHDR(str)) ) {
    RML_TAILCALLK(rmlFC);
  } else {
    rml_uint_t ch = ((unsigned char*)RML_STRINGDATA(str))[i];
    rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(ch));
    RML_TAILCALLK(rmlSC);
  }
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__string_5fnth_5fstring_5fchar)
{
  void *str = rmlA0;
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
  if( i >= RML_HDRSTRLEN(RML_GETHDR(str)) ) {
    RML_TAILCALLK(rmlFC);
  } 
  else 
  {
    struct rml_string *strnew = rml_prim_mkstring(1, 2);
    /* re-read after alloc, it may have been moved */
    unsigned char *snew = (unsigned char*)strnew->data;
    *snew++ = ((unsigned char*)RML_STRINGDATA(rmlA0))[i];
    *snew = '\0';
    rmlA0 = RML_TAGPTR(strnew);
    RML_TAILCALLK(rmlSC);
  }
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__string_5fget)
{
  void *str = rmlA0;
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
  if( i-1 >= RML_HDRSTRLEN(RML_GETHDR(str)) ) {
    RML_TAILCALLK(rmlFC);
  } else {
    rml_uint_t ch = ((unsigned char*)RML_STRINGDATA(str))[i-1];
    rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(ch));
    RML_TAILCALLK(rmlSC);
  }
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__string_5fget_5fstring_5fchar)
{
  void *str = rmlA0;
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1);
  if( i-1 >= RML_HDRSTRLEN(RML_GETHDR(str)) ) {
    RML_TAILCALLK(rmlFC);
  } 
  else 
  {
    struct rml_string *strnew = rml_prim_mkstring(1, 2);
    /* re-read after alloc, it may have been moved */
    unsigned char *snew = (unsigned char*)strnew->data;
    *snew++ = ((unsigned char*)RML_STRINGDATA(rmlA0))[i-1];
    *snew = '\0';
    rmlA0 = RML_TAGPTR(strnew);
    RML_TAILCALLK(rmlSC);
  }
}
RML_END_LABEL

/* str_setnth.c */
RML_BEGIN_LABEL(RML__string_5fsetnth)
{
  void *strold = rmlA0; /* string */
  rml_uint_t len = RML_HDRSTRLEN(RML_GETHDR(rmlA0)); /* string lenght */
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1); /* index */
  rml_uint_t ch = (rml_uint_t)RML_UNTAGFIXNUM(rmlA2); /* char */
  if( i >= RML_HDRSTRLEN(RML_GETHDR(strold)) ) 
  {
    RML_TAILCALLK(rmlFC);
  } 
  else 
  {
    /* first copy the old string */
    struct rml_string *strnew = rml_prim_mkstring(len, 3);
    /* re-read after alloc, it may have been moved */
    strold = rmlA0;
    unsigned char *sold = (unsigned char*)RML_STRINGDATA(strold);
    unsigned char *snew = (unsigned char*)strnew->data;
    rmlA0 = RML_TAGPTR(strnew);
    for(; len > 0; --len)
      *snew++ = *sold++;
    /* update the char */
    *snew = '\0';
    RML_STRINGDATA(rmlA0)[i] = ch;
  }
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


RML_BEGIN_LABEL(RML__string_5fupdate)
{
  void *strold = rmlA0; /* string */
  rml_uint_t len = RML_HDRSTRLEN(RML_GETHDR(rmlA0)); /* string lenght */
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1); /* index */
  rml_uint_t ch = RML_UNTAGFIXNUM(rmlA2); /* char */
  if( i-1 >= RML_HDRSTRLEN(RML_GETHDR(strold)) ) 
  {
    RML_TAILCALLK(rmlFC);
  } 
  else 
  {
    /* first copy the old string */
    struct rml_string *strnew = rml_prim_mkstring(len, 3);
    /* re-read after alloc, it may have been moved */
    strold = rmlA0;
    unsigned char *sold = (unsigned char*)RML_STRINGDATA(strold);
    unsigned char *snew = (unsigned char*)strnew->data;
    rmlA0 = RML_TAGPTR(strnew);
    for(; len > 0; --len)
      *snew++ = *sold++;
    *snew = '\0';
    /* update the char */
    RML_STRINGDATA(rmlA0)[i-1] = ch;
  }
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

/* adrpo added setting of a string entry with a string char */
RML_BEGIN_LABEL(RML__string_5fsetnth_5fstring_5fchar)
{
  void *strold = rmlA0; /* string */
  rml_uint_t len = RML_HDRSTRLEN(RML_GETHDR(rmlA0)); /* string lenght */
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1); /* index */
  rml_uint_t ch = RML_STRINGDATA(rmlA2)[0]; /* char */
  if( i >= RML_HDRSTRLEN(RML_GETHDR(strold)) ) 
  {
    RML_TAILCALLK(rmlFC);
  } 
  else 
  {
    /* first copy the old string */
    struct rml_string *strnew = rml_prim_mkstring(len, 3);
    /* re-read after alloc, it may have been moved */
    strold = rmlA0;
    unsigned char *sold = (unsigned char*)RML_STRINGDATA(strold);
    unsigned char *snew = (unsigned char*)strnew->data;
    rmlA0 = RML_TAGPTR(strnew);
    for(; len > 0; --len)
      *snew++ = *sold++;
    *snew = '\0';
    /* update the char */
    RML_STRINGDATA(rmlA0)[i] = ch;
  }
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL


RML_BEGIN_LABEL(RML__string_5fupdate_5fstring_5fchar)
{
  void *strold = rmlA0; /* string */
  rml_uint_t len = RML_HDRSTRLEN(RML_GETHDR(rmlA0)); /* string lenght */
  rml_uint_t i = (rml_uint_t)RML_UNTAGFIXNUM(rmlA1); /* index */
  rml_uint_t ch = RML_STRINGDATA(rmlA2)[0]; /* char */
  if( i-1 >= RML_HDRSTRLEN(RML_GETHDR(strold)) ) 
  {
    RML_TAILCALLK(rmlFC);
  } 
  else 
  {
    /* first copy the old string */
    struct rml_string *strnew = rml_prim_mkstring(len, 3);
    /* re-read after alloc, it may have been moved */
    strold = rmlA0;
    unsigned char *sold = (unsigned char*)RML_STRINGDATA(strold);
    unsigned char *snew = (unsigned char*)strnew->data;
    rmlA0 = RML_TAGPTR(strnew);
    for(; len > 0; --len)
      *snew++ = *sold++;
    *snew = '\0';
    /* update the char */
    RML_STRINGDATA(rmlA0)[i-1] = ch;
  }
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__string_5fequal)
{
  char *str1 = RML_STRINGDATA(rmlA0);
  char *str2 = RML_STRINGDATA(rmlA1);
  rml_uint_t len1 = strlen(str1);
  rml_uint_t len2 = strlen(str2);
  if (len1 != len2) 
  {
    rmlA0 = RML_FALSE;
    RML_TAILCALLK(rmlSC);
  } 
  if( !memcmp(str1, str2, len1) )
    rmlA0 = RML_TRUE;
  else
    rmlA0 = RML_FALSE;
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

RML_BEGIN_LABEL(RML__string_5fcompare)
{
  char *str1 = RML_STRINGDATA(rmlA0);
  char *str2 = RML_STRINGDATA(rmlA1);
  rml_sint_t result = strcmp(str1, str2);
  rmlA0 = RML_IMMEDIATE(RML_TAGFIXNUM(result));
  RML_TAILCALLK(rmlSC);
}
RML_END_LABEL

