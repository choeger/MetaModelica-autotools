/* repchk.c */
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>	/* exit() */
#include <stddef.h>	/* size_t and offsetof() */
#ifdef CRAP_OFFSETOF
#undef offsetof
#define offsetof(TYPE,MEMBER) ((size_t)((char*)&(((TYPE*)0)->MEMBER) - (char*)0))
#endif
#ifndef NO_UNIX_SIGNALS
#include <signal.h>
#include <setjmp.h>
#endif

typedef unsigned long myulong;	/* <sys/types.h> often defines "ulong" .. */

void fatal(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "repchk error: ");
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit(1);
}

#define ALIGN(TYPE)	offsetof(struct{char c; TYPE y;}, y)
#define OFF2DBL(TYPE)	offsetof(struct{TYPE x; double y;}, y)

struct word_type {
    char	*name;
    myulong	size;
    myulong	ptrsiz;
    unsigned	align;
    unsigned	off2dbl;
} word_types[] = {
    { "int",	sizeof(int),	sizeof(int*),	ALIGN(int),	OFF2DBL(int)	},
    { "long",	sizeof(long),	sizeof(long*),	ALIGN(long),	OFF2DBL(long)	},
};

unsigned find_word_type(void)
{
    unsigned n_word_types = sizeof word_types / sizeof(struct word_type);
    unsigned wty;

    for(wty = 0; wty < n_word_types; ++wty)
	if( word_types[wty].size == sizeof(void*) )
	    return wty;
    fatal("no integer type has same size as the generic pointer type\n");
    /*NOTREACHED*/
    return 0;
}

unsigned log2_size(unsigned wty)
{
    myulong size, bit;
    unsigned n;

    size = word_types[wty].size;
    for(n = 0, bit = 1; bit; ++n, bit <<= 1)
	if( bit == size )
	    return n;
    fatal("the size of %s (%lu) is not a power of two\n",
	  word_types[wty].name, size);
    /*NOTREACHED*/
    return 0;
}

void *tag(void *p)
{
    return (void*)((myulong)p + 3);
}

void *untag(void *p)
{
    return (void*)((myulong)p - 3);
}

int asr1(int x)
{
    return x >> 1;	/* ANSI-C doesn't require this to work when x < 0 */
}

void check_basics(unsigned wty)
{
    typedef void (*PFV)(void);
    struct S { char x; } s;

    if( sizeof(void*) != sizeof(struct S*) )
	fatal("generic and structure pointers differ in size (%lu != %lu)\n",
	      (myulong)sizeof(void*), (myulong)sizeof(struct S*));
    if( sizeof(void*) != word_types[wty].ptrsiz )
	fatal("generic and %s pointers differ in size (%lu != %lu)\n",
	      word_types[wty].name, (myulong)sizeof(void*), word_types[wty].ptrsiz);
    if( sizeof(void*) != sizeof(PFV) )
	fatal("generic and function pointers differ in size (%lu != %lu)\n",
	      (myulong)sizeof(void*), (myulong)sizeof(PFV));

    if( (struct S*)untag(tag((void*)&s)) != &s )
	fatal("arithmetic on generic pointers doesn't work (0x%lx != 0x%lx)\n",
	      (myulong)untag(tag((void*)&s)), (myulong)&s);
    if( (PFV)(void*)check_basics != (PFV)check_basics )
	fatal("function pointers cannot be cast to generic pointers safely\n");

    if( word_types[wty].align & 0x3 )
	fatal("%s objects are not sufficiently aligned\n",
	      word_types[wty].name);
    if(   (myulong)check_basics & 0x1
       || (myulong)untag & 0x1
       || (myulong)tag & 0x1
       || (myulong)find_word_type & 0x1
       || (myulong)fatal & 0x1
       )
	fatal("function addresses are not at least evenly aligned\n");

    if( asr1(-1 << 1) != -1 )
	fatal("right shift of negative value doesn't work");
}

void add(double *dst, double *src1, double *src2)
{
    *dst = *src1 + *src2;
}

#ifndef NO_UNIX_SIGNALS
jmp_buf catch;

void onbus(int sig_ignored)
{
    longjmp(catch, 1);
}
#endif

double blob[4];	/* must start out as all-bits-zero */
double *word_aligned_ptr;

void check_doubles(unsigned wty, unsigned *dbl_pad, unsigned *dbl_strict)
{
    myulong word_size;

    word_size = word_types[wty].size;
    if( sizeof(double) != word_size && sizeof(double) != 2*word_size )
	fatal("the size of a double (%lu) isn't 1 or 2 times the size of %s (%lu)\n",
	      (myulong)sizeof(double), word_types[wty].name, word_size);
    if( word_types[wty].off2dbl == word_types[wty].size )
	*dbl_pad = 0;
    else
	*dbl_pad = 1;
#ifdef NO_UNIX_SIGNALS
    /* conservatively assume that padding implies strictness */
    *dbl_strict = *dbl_pad ? 2 : 0;
#else
    /* construct a word- but possibly not double-aligned address */
    word_aligned_ptr = (double*) ((myulong)blob | word_size);
    signal(SIGBUS, onbus);
    if( setjmp(catch) == 0 ) {
	add(word_aligned_ptr+0, word_aligned_ptr+1, word_aligned_ptr+2);
	*dbl_strict = 0;
    } else
	*dbl_strict = 1;
    if( *dbl_pad == 0 && *dbl_strict != 0 )
	fatal("the compiler is inconsistent: it does not align doubles\nin structures even though it requires double alignment for accesses\n");
#endif
}

unsigned check_stack2(char *c0, char *c1)
{
    char c2;

    if( &c2 < c1 && c1 < c0 )
	return 0;
    if( &c2 > c1 && c1 > c0 )
	return 1;
    fatal("weird stack\n");
    /*NOTREACHED*/
    return 0;
}

unsigned check_stack1(char *c0)
{
    char c1;
    return check_stack2(c0, &c1);
}

unsigned check_stack(void)
{
    char c0;
    return check_stack1(&c0);
}

int main(void)
{
    unsigned wty;
    unsigned log2_size_int;
    unsigned dbl_pad;
    unsigned dbl_strict;
    unsigned stack_up;

    wty = find_word_type();
    log2_size_int = log2_size(wty);
    check_basics(wty);
    check_doubles(wty, &dbl_pad, &dbl_strict);
    stack_up = check_stack();

    printf("/* the following section was generated by repchk */\n");
    printf("typedef %s rml_sint_t;\n", word_types[wty].name);
    printf("typedef unsigned %s rml_uint_t;\n", word_types[wty].name);
    printf("#define RML_LOG2_SIZE_INT %u\n", log2_size_int);
    printf("#define RML_SIZE_INT %lu\n", word_types[wty].size);
    printf("#define RML_SIZE_DBL %lu\n", (myulong) sizeof(double));
    if( dbl_pad )
	printf("#define RML_DBL_PAD\n");
    switch( dbl_strict ) {
      case 1:
	printf("#define RML_DBL_STRICT\n");
	break;
      case 2:
	printf("#define RML_DBL_STRICT /* conservative assumption */\n");
    }
    if( stack_up )
	printf("#define RML_STACK_GROWS_POSITIVE\n");
    printf("/* end of repchk-generated section */\n");
    return 0;
}
