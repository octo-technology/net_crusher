#!/bin/sh -e

if [ -f sinatra.pid ]; then
  thin -P sinatra.pid stop
fi

port1=$(ruby find_free_tcp_port.rb)
port2=$(ruby find_free_tcp_port.rb)

thin -d -P sinatra.pid -p $port1 start

sleep 1

echo $port1 > sinatra.port
echo $port2 > unused.port

echo "Server ready on port $port, pid `cat sinatra.pid`"

