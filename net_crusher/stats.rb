#!/usr/bin/env ruby

puts "Parse log files #{ARGV}"

stats = {}
valuelist = {}
nb = 0

ARGV.each do |file|
  File.open(file).each_line{ |s|
    if s =~ /(\d+) (\d+) \S+ (\S+)/
      start, delay, id = $1, $2, $3
      delay = delay.to_f / 1000
      stats[id] = {:count => 0, :min => 1073741823, :max => 0, :sum => 0} unless stats[id]
      valuelist[id] = [] unless valuelist[id]
      stats[id][:count] = stats[id][:count] + 1
      stats[id][:sum] = stats[id][:sum] + delay
      stats[id][:min] = delay if delay < stats[id][:min]
      stats[id][:max] = delay if delay > stats[id][:max]
      valuelist[id] << delay
    else
      raise "Wrong line #{s}"
    end
  }
end

def calculate_percentile(array, percentile)
  array.sort[(percentile * array.length).ceil - 1]
end

stats.each do |id, s|
  puts "#{id};count=#{s[:count]};min=#{s[:min]} ms;max=#{s[:max]} ms; avg=#{s[:sum] / s[:count]} ms;#{calculate_percentile(valuelist[id], 0.9)} (%90)"
end
