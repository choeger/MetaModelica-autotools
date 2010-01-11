# variables for SPARC / Solaris 2.x / gcc
CC=gcc
# register g5 is only used by the `pushy' runtime
#CFLAGS=-ffixed-g5
CFLAGS=
COFLAGS=-O2
CPFLAGS=-pg
CGFLAGS=-g -DRML_DEBUG
RANLIB=echo
CPP=gcc -x c -E -ansi
AS=/usr/ccs/bin/as
LD=/usr/ccs/bin/ld
