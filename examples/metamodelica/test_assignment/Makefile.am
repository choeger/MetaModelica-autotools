AM_YFLAGS=-d
bin_PROGRAMS=calc

calc_SOURCES=main.c parser.y lexer.l yacclib.c yacclib.h Assignment.c

BUILT_SOURCES=Assignment.c Assignment.h
CLEAN_FILES=$(BUILD_SOURCES)

Assignment.c Assignment.h: Assignmnent.mo
	$(RML_C) Assignment.mo
