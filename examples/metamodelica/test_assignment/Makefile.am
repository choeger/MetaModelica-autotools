AM_YFLAGS=-d
bin_PROGRAMS=calc

calc_SOURCES=main.c parser.y lexer.l yacclib.c yacclib.h assignment.c

BUILT_SOURCES=assignment.c assignment.h
CLEAN_FILES=$(BUILD_SOURCES)

assignment.c assignment.h: assignment.mo
	$(RMLC) assignment.mo

test:
	@echo "Test successfull!"
