include $(top_srcdir)/config/$(TARGET)/variables.mk
bin_SCRIPTS=rmlc

CLEANFILES=rmlc

rmlc: skel.rmlc
	echo "#variables from build environment:" > rmlc
	echo "PREFIX=$(DESTDIR)" >> rmlc
	echo "COFLAGS=$(COFLAGS)" >> rmlc
	echo "CPFLAGS=$(CPFLAGS)" >> rmlc
	echo "CGFLAGS=$(CGFLAGS)" >> rmlc
	echo "CC=$(CC)" >> rmlc
	echo "" >> rmlc	
	cat skel.rmlc >> rmlc


