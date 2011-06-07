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

require 'yaml'
config_file = ARGV[0]

puts "Using config file #{config_file}"

config = YAML.load(File.read(config_file))

unless config["slaves"]
  puts "No slaves found in #{config_file}"
  exit 1
end

slaves = config["slaves"]

puts "Slaves : #{slaves.inspect}"

path_to_rsync = File.expand_path(File.dirname(__FILE__))

puts "Synchronize #{path_to_rsync}"

slaves.each do |s|
  cmd = "rsync -avh -e ssh #{path_to_rsync} root@#{s}:#{path_to_rsync}"
  puts "Executing #{cmd}"
  puts %x{#{cmd}}
end

puts "Done."
