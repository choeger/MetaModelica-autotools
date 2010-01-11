/* labtab.h for SPARC / SunOS 4 */
#ifdef __STDC__
#define RML_GLUE(A,B) A##B
#else
#define RML_GLUE(A,B) A/**/B
#endif
#define RML_LABTAB_SETUP(FUN)		.text; .align 4
#define RML_LABTAB_ENTRY(LAB,FUN)	.global RML_GLUE(_,LAB); RML_GLUE(_,LAB): .word RML_GLUE(_,FUN)
