lib_LTLIBRARIES=librml_plain.la

RMLHSRC=../common/rml-start.h ../common/rml-core.h rml-labels.h ../common/rml-predef.h\
        ../debugging/command.h ../debugging/socket.h ../debugging/handler.h ../debugging/load.h \
        ../debugging/debug.h ../common/rml-end.h

librml_plain_la_SOURCES= ../common/array.c ../common/boolean.c ../common/integer.c ../common/list.c ../common/logicalvar.c ../common/real.c ../common/string.c ../common/vector.c ../common/internal.c \
 ../common/p-gccore.c ../common/p-gcuser-plain.c ../common/p-usergc.c ../common/controlflow.c \
 ../debugging/parse-command.c ../debugging/scan-command.c ../debugging/parse-rdb.c ../debugging/scan-rdb.c ../debugging/load.c \
 ../debugging/socket.c ../debugging/command.c ../debugging/handler.c ../debugging/debug.c \
../common/main.c

BUILT_SOURCES=rml.h
CLEANFILES = rml.h

# create the platform specific include header 
rml.h:  ../../config.status rml-select.h ../../config/$(TARGET)/config.h $(RMLHSRC)
	echo '#include "config.h"' > rml.h
	cat rml-select.h ../../config/$(TARGET)/config.h $(RMLHSRC) >> rml.h


