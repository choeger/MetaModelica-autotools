/* labtab.h for x86 / Linux */
#define RML_LABTAB_SETUP(FUN)		.section .rodata; .align 4
#define RML_LABTAB_ENTRY(LAB,FUN)	.globl LAB; LAB: .long FUN
