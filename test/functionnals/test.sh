#!/bin/sh -e

cd `dirname $0`

TO_BE_TESTED=`find . -type d -name '*_*' | perl -pe 's/\.\/(.*)/$1/'`
ERLANG_CMD="../../../ebin/net_crusher"
TMP_FILE="/tmp/tmp_output.txt"

if [ "$TEST_FUNC" != "" ]; then
	TO_BE_TESTED=$TEST_FUNC
fi

for i in $TO_BE_TESTED; do
	echo "Running test in $i"
	cd $i
	if [ -f "output_erl.txt" ]; then
		echo "Processing erlang test for $i"
		$ERLANG_CMD $i.rb | perl -pe 's/^.*\s\[\s*[^[]*(\[\s*.*)$/$1/' | perl -pe 's/{node,[^}]+}/{node}/g' | perl -pe 's/yeccpars[0-9_]*/yeccpars/g' | perl -pe 's/{line,\d+}/{line,XXX}/g' > $TMP_FILE
		diff -du output_erl.txt $TMP_FILE || exit 1
	fi
	echo "Test ok : $i"
	cd ..
done
