ACLOCAL_AMFLAGS=-I m4
SUBDIRS=compiler runtime rmlc

TESTSUITE=./run_tests.sh

TESTSUITEFLAGS=$(prefix)

# Run the test suite on the *installed* tree.
installcheck-local: $(TESTSUITE)
	$(SHELL) $(TESTSUITE) $(TESTSUITEFLAGS)
