AC_INIT(rml_test_assignment, 2.3.8)
AM_INIT_AUTOMAKE
AC_PROG_CC

AC_PROG_LEX
AC_PROG_YACC

AC_PATH_PROG(RMLC, rmlc)
 
if test "z$RMLC" == "z"; then
	AC_MSG_ERROR([Cannot find rmlc])
fi 
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
