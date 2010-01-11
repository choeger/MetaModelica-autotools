/* pushy.c -- figure out stack overhead for `pushy' runtime */
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

char *stack_top;
unsigned numrecur;
jmp_buf main_buf;

/* dry-run */

void test0final(void)
{
    char sp;
    unsigned framesize = (unsigned)abs(stack_top - &sp) / numrecur;
    printf("frame size w. no locals == %u bytes\n", framesize);
    longjmp(main_buf, 1);
}

void test0recur(unsigned i)
{
    if( i > 0 )
	test0recur(i-1);
    test0final();
}

void test0(void)
{
    char sp;
    stack_top = &sp;
    test0recur(numrecur);
}

/* with 32 bytes allocated */

void test1final(char *foo)
{
    char sp;
    unsigned framesize = (unsigned)abs(stack_top - &sp) / numrecur;
    printf("frame size w. 32 bytes of locals == %u bytes\n", framesize);
    longjmp(main_buf, 2);
}

void test1recur(unsigned i, char *foo)
{
    char bar[32];
    if( i > 0 )
	test1recur(i-1, bar);
    test1final(bar);
}

void test1(void)
{
    char sp;
    stack_top = &sp;
    test1recur(numrecur, (char*)0);
}

int main(int argc, char **argv)
{
    numrecur = (argc == 2) ? (unsigned)atoi(argv[1]) : 100;
    switch( setjmp(main_buf) ) {
      case 0:	test0();
      case 1:	test1();
    }
    return 0;
}
