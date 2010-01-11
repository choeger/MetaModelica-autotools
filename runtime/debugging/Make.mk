#
# Makefile for the debug part of the runtime
#
GOROOT=../..

GENERATED=parse-rdb.c scan-rdb.c parse-command.c scan-command.c
			
default: parse-command.c scan-command.c parse-rdb.c scan-rdb.c

parse-command.c scan-command.h : parse-command.y
	bison -p aa --defines --output-file=parse-command.c parse-command.y

scan-command.c : scan-command.l parse-command.h
	flex -I -Paa scan-command.l

parse-rdb.c scan-rdb.h : parse-rdb.y
	bison -p aarmldb --defines --output-file=parse-rdb.c parse-rdb.y

scan-rdb.c : scan-rdb.l parse-rdb.h
	flex -Paarmldb scan-rdb.l

install: default
	echo "Nothing"

Makefile:	$(GOROOT)/config.cache Make.mk
	echo include $(GOROOT)/config.cache > Makefile
	. $(GOROOT)/config.cache; cat $(GOROOT)/config/$$TARGET/variables.mk >> Makefile
	cat Make.mk >> Makefile

configure:	clean-configure Makefile default

clean-configure:	clean
	rm -f Makefile

distclean:	realclean clean-configure
realclean:	clean

clean:
	rm -f *.o core rmldb \
	      a.out core mon.out gmon.out *~ $(GENERATED)


