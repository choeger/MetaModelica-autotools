#!/bin/sh
# This is a simple hand crafted script to test the installed tarball

runSingleTestcase() {
	TESTCASE=$1
	PWD=$(pwd)
	echo "#################################################################################"
	echo "Running testcase: $N"
	echo "#################################################################################"
	cd $TESTCASE
	(./test.sh $PREFIX && cd $PWD) || (echo "Error in $TESTCASE" && cd $PWD && return 1)
	return 0	
}

PREFIX=$1
echo "Testing installation. Prefix is: $PREFIX"
for N in $(find -name test_\* -and -type d); do
	if [ -d $N ]; then
		runSingleTestcase $N
	fi
done
