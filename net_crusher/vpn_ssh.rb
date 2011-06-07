#!/usr/bin/env ruby
## NetCrusher.
## Copyright (C) 2011 Bertrand Paquet, David Rousselie All Rights Reserved

## NetCrusher is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public
## License as published by the Free Software Foundation; either
## version 2.1 of the License, or (at your option) any later version.

## NetCrusher is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.

## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

require 'tempfile'

IP_PREFIX="10.199."
nodes = ARGV

def kill_all_tunnel
  %x{ps axu | grep ssh | grep '\\-w' | awk '{print $2}' | xargs kill}
end

def update_etc_hosts_commd config, last_line
  cmd = <<-CMD
  mv /etc/hosts /tmp/hosts
  cat /tmp/hosts | grep -v #{IP_PREFIX} > /etc/hosts
CMD
  config.each do |name, node|
     cmd = cmd + "echo \"#{node[:ip]} #{node[:hostname]}\" >> /etc/hosts\n"
   end
  cmd = cmd + "echo \"#{last_line}\" >> /etc/hosts\n"
  cmd
end

def establish_tunnel name, node
  Kernel.fork do
    puts "Starting tunnel to #{name}, using tun#{node[:tun]}"
    Kernel.exec "ssh -NTC -w #{node[:tun]}:0 #{name}"
  end
  sleep 2
  exec_local "ifconfig tun#{node[:tun]} #{node[:local_ip]} pointopoint #{node[:ip]}"
  exec_ssh name, "ifconfig tun0 #{node[:ip]} pointopoint #{node[:local_ip]}"
end

def exec_local cmd
  stdout = %x{#{cmd}}
  raise "Wrong exit code #{$?} output : #{stdout}" if $? != 0
  return stdout.strip
end

def build_config nodes
  config = {}

  inc = 0
  
  nodes.each do |node|
    cmd = <<-CMD
cat /etc/ssh/sshd_config | grep "PermitTunnel" > /dev/null
if [ "$?" -ne "0" ]; then
  echo "Please add PermitTunnel in /etc/ssh/sshd_config"
  exit 1
fi
ifconfig tun0 > /dev/null 2>&1
if [ "$?" -eq "0" ]; then
  echo "tun0 already exists"
  exit 1
fi
cat /etc/hostname
CMD
    hostname = exec_ssh(node, cmd)
    config[node] = {}
    config[node][:hostname] = hostname
    config[node][:ip] = IP_PREFIX + inc.to_s + ".2"
    config[node][:local_ip] = IP_PREFIX + inc.to_s + ".1"
    config[node][:tun] = inc
    inc = inc + 1
  end
  config
end

def exec_ssh node, cmd
  f = Tempfile.new("ssh")
  f.puts(cmd)
  f.close
  stdout = %x{cat #{f.path} | ssh -T #{node}}
  raise "Wrong exit code #{$?} on host #{node}, output : #{stdout}" if $? != 0
  return stdout.strip
end

local_name = File.read("/etc/hostname").strip
kill_all_tunnel
config = build_config nodes
config.each do |name, node|
  establish_tunnel name, node
  routes = ""
  config.each do |name2, node2|
    unless name == name2
      routes += "route add #{node2[:ip]} gw #{node[:local_ip]} tun0\n"
    end
  end
  exec_ssh name, update_etc_hosts_commd(config , "#{node[:ip]} #{local_name}\n") + routes
end
exec_local update_etc_hosts_commd(config, "")
exec_local "echo 1 > /proc/sys/net/ipv4/ip_forward"
puts "Done."

puts "Master name : #{local_name}"
config.each do |name, node|
  puts "Node        : #{node[:hostname]}"
end
