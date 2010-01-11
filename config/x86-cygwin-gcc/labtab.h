/* labtab.h for x86 / Cygwin */
#define GLUE(A,B) A##B
#define RML_LABTAB_SETUP(FUN)		.section .rodata; .align 4
#define RML_LABTAB_ENTRY(LAB,FUN)	.globl GLUE(_,LAB); GLUE(_,LAB): .long GLUE(_,FUN)
