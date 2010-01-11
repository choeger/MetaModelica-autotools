# variables for MIPS / Irix 6 / gcc
CC=gcc
CFLAGS=
COFLAGS=-O2
# got no libprof.a
CPFLAGS=
# no -g flags at all supported by this gcc
CGFLAGS=-DRML_DEBUG
RANLIB=echo
CPP=gcc -x c -E -ansi
AS=/usr/bin/as -nocpp
LD=/usr/bin/ld
