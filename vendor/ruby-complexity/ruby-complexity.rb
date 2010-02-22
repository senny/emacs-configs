#!/bin/env ruby

# DESCRIPTION:
#   Convert flog output to something that can be used by
#   pycomplexity.el
#
# AUTHOR:
#   Geoffrey Grosenbach http://peepcode.com
#
# USAGE:
#   cat file.rb | flog | ruby-complexity.rb
#
# INPUT:
#   27.1: Page#categories                  -:261
# OUTPUT:
#   261 263 27 Page#categories

def run
  results = {}

  IO.popen("flog -a #{ARGV[0]}").readlines.each do |line|
    # 27.1: Page#categories                  -:261
    if line.match(/([0-9.]+): (\S+)\s+[^:]+:(\d+)/)
      score = $1.to_f.ceil
      method_name = $2
      line_number = $3.to_i
      results[line_number] = [score, method_name]
    end
  end

  results.keys.sort {|a,b| a <=> b }.each do |line_number|
    # TODO: Find range of line numbers for each method
    puts [line_number,
          line_number,
          results[line_number][0],
          "function" #results[line_number][1]
         ].join(' ')
  end
end

run
