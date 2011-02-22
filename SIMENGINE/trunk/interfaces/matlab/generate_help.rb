#!/usr/bin/env ruby

class State
  INIT = 0
  CLASS = 1
  CLASSHELP = 2
  PROPERTIES = 3
  METHODS = 4
  FUNCTION = 5
  FUNCTIONHELP = 6
end

def parse_file (infile, outfile)
  puts "Parsing file #{infile} and creating file #{outfile}"

  count = 0;

  out = File.open(outfile, 'w');
  state = State::INIT;

  stack = 0;

  IO.readlines(infile).each {|orig_line|

    # get rid of extra space before and after
    line = orig_line.strip;

    if line =~ /^$/ then
      # ignore, blank line
    elsif line =~ /^classdef/ then
      state = State::CLASSHELP;
      out.puts(orig_line);
    elsif state == State::CLASSHELP then
      if line =~ /^%/ then
        out.puts(orig_line);
      else
        state = State::CLASS;
      end
    end
    
    if state == State::CLASS then
      if line =~ /^properties/ then
        state = State::PROPERTIES;
      elsif line =~ /^methods/ then
        out.puts(orig_line);
        state = State::METHODS;
      elsif line =~ /^end/ then
        out.puts(orig_line);
        state = State::INIT;
      end
    elsif state == State::PROPERTIES then
      if line =~ /^end/ then
        state = State::CLASS;
      end
    elsif state == State::METHODS then
      if line =~ /^function/ then
        out.puts(orig_line);
        state = State::FUNCTIONHELP;
      elsif line =~ /^end/ then
        out.puts(orig_line);
        state = State::CLASS;
      end
    elsif state == State::FUNCTIONHELP then
      if line =~ /^%/ then
        out.puts(orig_line);
      else
        state = State::FUNCTION;
      end
    end

    if state == State::FUNCTION
      if line =~ /^switch/ or line =~ /^if/ or line =~ /^for/ or line =~ /^while/ or line =~ /^function/ then
        stack += 1;
      elsif line =~ /^end/ 
        if stack == 0 then
          out.puts(orig_line);
          state = State::METHODS;
        else
          stack -= 1;
        end
      end
    end

    count += 1;
  }
  puts "Read #{count} lines"
end


if ARGV.size != 2 then
  puts "Wrong number of arguments"
  puts "Usage: generate_help orig.m orig_help.m"
else
  parse_file(ARGV[0], ARGV[1])
end
