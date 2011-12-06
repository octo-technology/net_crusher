
all: start test stop

start:
	./start.sh
  
stop:
	[ -f sinatra.pid ] && kill `cat sinatra.pid` && rm sinatra.pid

test:
	./test.sh
