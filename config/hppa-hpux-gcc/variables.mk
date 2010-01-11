# variables for HP-PA / HP-UX 9 / gcc
CC=gcc
CFLAGS=
COFLAGS=-O2
CPFLAGS=-pg
CGFLAGS=-g -DRML_DEBUG
RANLIB=echo
CPP=gcc -x c -E -ansi
AS=/bin/as
LD=/bin/ld
ASLABTABFIX=| tr '!' '\012'
