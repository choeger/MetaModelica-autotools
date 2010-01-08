AC_INIT(MetaModelica, 2.3.8)
AC_CONFIG_HEADER(include/config.h)
AM_INIT_AUTOMAKE

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S

# Check for dependencies

AC_PATH_PROG(MLTON, mlton)
if test "z$MLTON" == "z"; then
	AC_MSG_ERROR([Cannot find mlton])
fi

AC_PATH_PROG(MLYACC, mlyacc)
if test "z$MLTON" == "z"; then
	AC_MSG_ERROR([Cannot find mlyacc])
fi

AC_PATH_PROG(MLLEX, mllex)
if test "z$MLTON" == "z"; then
	AC_MSG_ERROR([Cannot find mllex])
fi


AC_CONFIG_FILES( [Makefile compiler/Makefile])
AC_OUTPUT
