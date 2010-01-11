# variables for RS6000 / PARIX 1.x / gcc
RUN=/px/parix/bin/px run -an4 1 1
AR=/px/ppc/bin/ar
CC=/px/gnu/bin/gcc
CFLAGS=
COFLAGS=-O2 -mno-stack-check
# neither profiling nor debugging seems to be available
CPFLAGS=
CGFLAGS=-DRML_DEBUG
RANLIB=echo
CPP=/px/gnu/bin/gcc -x c -E -traditional-cpp
AS=/px/ppc/bin/as -Q
LD=/px/ppc/bin/ld -bbinder:/px/ppc/bin/mbind
