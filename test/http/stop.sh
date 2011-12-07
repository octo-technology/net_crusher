#!/bin/sh -e

thin -d -p sinatra.pid stop
rm sinatra.pid