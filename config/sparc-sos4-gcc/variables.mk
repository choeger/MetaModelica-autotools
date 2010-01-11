# variables for SPARC / SunOS 4.1.x / gcc
CC=gcc
# register g5 is only used by the `pushy' runtime
#CFLAGS=-ffixed-g5
CFLAGS=
COFLAGS=-O2
CPFLAGS=-pg
CGFLAGS=-g -DRML_DEBUG
RANLIB=ranlib
CPP=gcc -x c -E -ansi
AS=/usr/bin/as
LD=/usr/bin/ld
