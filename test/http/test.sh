#!/bin/sh -e

cd `dirname $0`

TO_BE_TESTED=`find . -name 'test_*.rb' | perl -pe 's/\.\/(.*)/$1/'`
ERLANG_CMD="../../ebin/net_crusher.escript"
PORT=`cat sinatra.port`
UNUSED_PORT=`cat unused.port`

if [ "$TEST_HTTP" != "" ]; then
	TO_BE_TESTED=$TEST_HTTP
fi

for i in $TO_BE_TESTED; do
	echo "Running test in $i"
	$ERLANG_CMD $i port=$PORT unused_port=$UNUSED_PORT
	echo "Test ok : $i"
done
