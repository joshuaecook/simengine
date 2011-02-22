#!/bin/env ruby
# Copyright 2009 Simatra Modeling Technologies

require 'rubygems'
require 'commandline'

class Simex < CommandLine::Application

  # instance variables
  @modelname = "" # model name
  @buildpath = "" # what SIMENGINE needs to be set to

  # Create a simEngine Helper class
  def simEngineHelper
    
    scriptpath = File.expand_path(File.dirname(__FILE__))
    @buildpath = scriptpath + '/build'
    simenginepath = @buildpath + '/bin/simEngine'
    
    # Verify the existence of simEngine
    if !(FileTest.file? simenginepath) then
      puts "Error: Can't find simEngine (#{simenginepath})"
      return false
    end
    
    # Verify the existence of the model file
    dslmodel = File.expand_path(@dsl_file)
    if !(FileTest.exist? dslmodel) then
      puts "Error: Can't find DSL model (#{dslmodel})"
      return false
    end

    # Grab model name
    if dslmodel =~ /.*\/(\S+)\.dsl$/ then
      @modelname = $1
    else
      puts "Error: Can't pull out model name from dsl file.  DSL model must have a .dsl extension"
      return false
    end
    
    # Generate command to run simEngine
    command = "sh -c 'export SIMENGINE=#{@buildpath}; echo \"import \\\"#{dslmodel}\\\"\nprint(compile(#{@modelname}))\" | #{simenginepath} -batch 2>& 1'"
    puts "Running command: #{command}"
    output = `#{command}`
    puts output
    
    return true
  end

  # This function is will compile the generated C code into an executable
  def compileSimulation
    
    command = "gcc #{@modelname}.c -lm -I#{@buildpath}/include -DSIMENGINE_STORAGE_double -DTARGET_CPU -o #{@modelname}"
    output = `#{command} 2>& 1`
    
    # Search for errors in output
    foundError = false
    output.each_line {|line|
      if line =~ /error/ then
        foundError = true
        break
      end
    }

    # Only display output if an error was found
    if foundError then
      puts " "
      puts " "
      puts "GCC Command: #{command}"
      puts " "
      puts output
      return false
    end

    return true
  end

  # This will run the DSL model
  def runModel    
    puts "Running model: #{@modelname}"

    if !(compileSimulation) then
      puts "Error: Simulator compilation failed"
      return false
    end

    return true
  end

  # Initialize arguments
  def initialize
#    args = ARGV
    synopsis "[-dh] dsl_file"
    short_description "Runs the simEngine compiler and generated simulation engine"
    long_description "Given a DSL model description, #{name} runs the simEngine compilation engine.  Following compilation, a c file is generated from simEngine which is then compiled using gcc into an executable.  That executable is evaluated and the resulting output is displayed on the screen."
    expected_args :dsl_file
    options :help, :debug
    
  end

  # This is the main of the program.  Initialize is run prior to main.
  def main
#    puts "#{name} called with #{args.size} arguments: #{args.inspect}"
#    puts "@dsl_file = #{@dsl_file}"
#    puts "in main"

#    opts = parseOptions(args)
    if args.size > 0
      if !(simEngineHelper) then    
        puts "Error: simEngine did not run successfully"
        return false
      end
      
      if !(runModel) then
        puts "Error: simulation engine failed during execution"
        return false
      end
#    else
#      puts "Usage: simex.rb model.dsl [options ...]"
    end

  end

end # class simex

#s = Simex.new(ARGV)
