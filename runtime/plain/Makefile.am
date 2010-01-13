include $(top_srcdir)/config/$(TARGET)/variables.mk

AM_YFLAGS=-d
lib_LTLIBRARIES=librml_plain.la librml_plain_debug.la librml_plain_prof.la



RMLHSRC=../common/rml-start.h ../common/rml-core.h rml-labels.h ../common/rml-predef.h\
        ../debugging/command.h ../debugging/socket.h ../debugging/handler.h ../debugging/load.h \
        ../debugging/debug.h ../common/rml-end.h

CSOURCES= ../common/array.c ../common/boolean.c ../common/integer.c ../common/list.c ../common/logicalvar.c ../common/real.c ../common/string.c ../common/vector.c ../common/internal.c \
 ../common/p-gccore.c ../common/p-gcuser-plain.c ../common/p-usergc.c ../common/controlflow.c \
 ../debugging/parse-command.y ../debugging/scan-command.l ../debugging/parse-rdb.y ../debugging/scan-rdb.l ../debugging/load.c \
 ../debugging/socket.c ../debugging/command.c ../debugging/handler.c ../debugging/debug.c \
../common/main.c

librml_plain_la_SOURCES=$(CSOURCES)
librml_plain_debug_la_SOURCES=$(CSOURCES)
librml_plain_debug_la_CFLAGS=$(CGFLAGS)
librml_plain_prof_la_SOURCES=$(CSOURCES)
librml_plain_prof_la_CFLAGS=$(CFLAGS) $(CPFLAGS)


BUILT_SOURCES=rml.h parse-command.h parse-rdb.h
CLEANFILES = rml.h parse-command.h parse-command.c parse-rdb.h parse-rdb.c scan-command.c scan-rdb.c

# create the platform specific include header 
rml.h:  ../../config.status rml-select.h ../../config/$(TARGET)/config.h $(RMLHSRC)
	echo '#include "config.h"' > rml.h
	cat rml-select.h ../../config/$(TARGET)/config.h $(RMLHSRC) >> rml.h


