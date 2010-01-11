/* labtab.h for RS6000 / Parix */
#define RML_LABTAB_SETUP(FUN)		.extern FUN[DS]; .csect .data[RW]; .align 2
#define RML_LABTAB_ENTRY(LAB,FUN)	.globl LAB; LAB: .long FUN[DS]
