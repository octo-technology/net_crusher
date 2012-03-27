def parse_file(file) 
  valuelist = {}
  File.open(file).each_line do |line|
   if line.include?(";") 
     columns = line.split(";")
     id = columns[0] 
     avg = columns[4].match(/([\d\.]+)/)[1] 
     percent90 = columns[5].match(/([\d\.]+)/)[1]
     valuelist[id] = {:avg => avg, :percent90 => percent90}
  end
  end
  valuelist
end

def fill s, k
  (s.nil? ? "" : s.to_s).ljust(k)
end


valuelist1 = parse_file(ARGV[0])
valuelist2 = parse_file(ARGV[1])

puts "#{fill("id", 15)}#{fill("avg diff", 10)}#{fill("%", 10)}#{fill("p90 diff", 10)}#{fill("%", 10)}"
valuelist2.keys.each  do |key|
  pair2 = valuelist2[key]
  pair1 = valuelist1[key]
  avgdiff = pair2[:avg].to_f - pair1[:avg].to_f
  percent90diff = pair2[:percent90].to_f - pair1[:percent90].to_f
  avgdiff_percent = (pair1[:avg].to_f / pair2[:avg].to_f)*100
  percent90diff_percent = (pair1[:percent90].to_f / pair2[:percent90].to_f)*100
  puts "#{fill(key, 15)}#{fill(sprintf("%.2f", avgdiff), 10)}#{fill(sprintf("+%.2f%%", avgdiff_percent),10)}#{fill(sprintf("%.2f", percent90diff), 10)}#{fill(sprintf("+%.2f%%", percent90diff_percent), 10)}"
end

