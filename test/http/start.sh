#!/bin/sh -e

if [ -f sinatra.pid ]; then
  kill `cat sinatra.pid` || true
fi

port=$(ruby find_free_tcp_port.rb)

rm -f nohup.out
nohup ruby server.rb -p$port &

pid=''
while [ "$pid" = "" ]
do
  sleep 1
  pid=`cat nohup.out | grep 'WEBrick::HTTPServer#start' | perl -pe 's/.*pid=(.*) .*/$1/'`
done

echo $pid > sinatra.pid
echo $port > sinatra.port

echo "Server ready on port $port, pid $pid"

