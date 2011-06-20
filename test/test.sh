#!/bin/sh -e

cd `dirname $0`

DATA=data

TO_BE_TESTED=`ls $DATA`
ERLANG_CMD="elixir -pa ../../../ebin/"
TMP_FILE="/tmp/tmp_output.txt"

if [ "$1" != "" ]; then
	TO_BE_TESTED=$1
fi

for i in $TO_BE_TESTED; do
	echo "Running test in $i"
	cd $DATA/$i
	if [ -f "output_erl.txt" ]; then
		echo "Processing erlang test for $i"
		ERL_AFLAGS="-mnesia schema_location ram -eval 'error_logger:tty(false).'" $ERLANG_CMD $i.exs | grep -v "=ERROR REPORT====" | grep -v "Error in process"| perl -pe 's/^.*\s\[\s*[^[]*(\[\s*.*)$/$1/' | perl -pe 's/{node,[^}]+}/{node}/g' | perl -pe 's/yeccpars[0-9_]*/yeccpars/g' > $TMP_FILE
		diff -du output_erl.txt $TMP_FILE || exit 1
	fi
	echo "Test ok : $i"
	cd ../../
done
