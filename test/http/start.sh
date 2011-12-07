#!/bin/sh -e

if [ -f sinatra.pid ]; then
  thin -P sinatra.pid stop
fi

port=$(ruby find_free_tcp_port.rb)

thin -d -P sinatra.pid -p $port start

sleep 1

echo $port > sinatra.port

echo "Server ready on port $port, pid `cat sinatra.pid`"

