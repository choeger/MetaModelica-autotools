/* labtab.h for HP-PA / HP-UX 9 */
/* the output must be piped through "tr '!' '\012'" */
#define RML_LABTAB_SETUP(FUN)\
! .IMPORT FUN,CODE ! .SPACE $TEXT$ ! .SUBSPA $LIT$ ! .ALIGN 8
#define RML_LABTAB_ENTRY(LAB,FUN)\
! .EXPORT LAB,DATA ! .LABEL LAB ! .WORD FUN
