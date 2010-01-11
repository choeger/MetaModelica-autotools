AC_INIT(MetaModelica, 2.3.8)
AC_CANONICAL_SYSTEM
AC_CONFIG_HEADER(include/config.h)
AM_INIT_AUTOMAKE
AC_CONFIG_MACRO_DIR([m4])

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AC_PROG_LN_S
AC_PROG_LEX
AC_PROG_YACC

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


TARGET=$build_cpu-$build_os-$CC

if test ! -e config/$TARGET/config.h; then
	AC_MSG_ERROR(["Build system is $TARGET, but I could not find config/$TARGET/config.h"])
fi
AC_SUBST(TARGET)

AC_CONFIG_FILES( [Makefile compiler/Makefile runtime/Makefile runtime/plain/Makefile])
AC_OUTPUT
