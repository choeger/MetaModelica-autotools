/* labtab.h for PPC / Darwin 7.8.0 / gcc */
/* Vales are copied from x86 / linux / gcc. Seems to work */
#define RML_LABTAB_SETUP(FUN)		.section .rodata; .align 4
#define RML_LABTAB_ENTRY(LAB,FUN)	.globl LAB; LAB: .long FUN
