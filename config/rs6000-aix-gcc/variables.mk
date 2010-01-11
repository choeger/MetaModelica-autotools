# variables for RS6000 / AIX 4.1 / gcc
CC=gcc
CFLAGS=
COFLAGS=-O2
CPFLAGS=-pg
CGFLAGS=-g -DRML_DEBUG
RANLIB=echo
CPP=gcc -x c -E -traditional-cpp
AS=/usr/ccs/bin/as
LD=/usr/ccs/bin/ld
