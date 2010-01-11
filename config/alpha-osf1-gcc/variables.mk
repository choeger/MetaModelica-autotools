# variables for ALPHA-based DECstation / OSF1.x / gcc
CC=gcc
CFLAGS=
COFLAGS=-O2
CPFLAGS=-pg
CGFLAGS=-g -DRML_DEBUG
RANLIB=echo
CPP=gcc -x c -E -ansi
AS=/usr/bin/as -nocpp
LD=/usr/bin/ld
