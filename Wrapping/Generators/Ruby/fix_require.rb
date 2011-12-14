#!/usr/bin/env ruby

def fix_require(l)
  l = l.gsub('rb_require(', '// rb_require(')
  l = l.gsub('rb_require_wrapitk(', 'rb_require(')
  return l
end

input = File.new(ARGV[0])
output = File.new(ARGV[1], "w")
input.each_line {|line| output.puts( fix_require( line ) )}
input.close
output.close
