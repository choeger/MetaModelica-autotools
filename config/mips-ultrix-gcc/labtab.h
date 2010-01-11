/* labtab.h for MIPS / Ultrix 4 */
#define RML_LABTAB_SETUP(FUN)		.sdata; .align 2
#define RML_LABTAB_ENTRY(LAB,FUN)	.globl LAB; LAB: .word FUN
