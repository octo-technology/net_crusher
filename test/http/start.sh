#!/bin/sh -e

if [ -f sinatra.pid ]; then
  thin -p sinatra.pid stop
fi

port=$(ruby find_free_tcp_port.rb)

thin -d -s 1 -p sinatra.pid -p $port start

sleep 1

echo $pid > sinatra.pid
echo $port > sinatra.port

echo "Server ready on port $port, pid $pid"

