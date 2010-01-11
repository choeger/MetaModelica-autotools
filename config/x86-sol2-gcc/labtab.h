/* PRELIMINARY labtab.h for x86 / Solaris 2 */
#define RML_LABTAB_SETUP(FUN)		.section ".rodata"; .align 4
#define RML_LABTAB_ENTRY(LAB,FUN)	.global LAB; LAB: .word FUN
