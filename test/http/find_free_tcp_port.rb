require 'net/http'

server = TCPServer.new('127.0.0.1', 0)
port = server.addr[1]
server.close()

puts port