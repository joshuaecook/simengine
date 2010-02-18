namespace CommandLine
  function parseCommandLine() = parseOptions(getCommandLine())

  function printUsage()
    println("\nsimEngine usage:\n\n" +
	    "\tSimulation mode: run a simulation from a Diesel model\n" +
	    "\t\tsimEngine [options] -model <modelfile.dsl>\n\n" +
	    "\tBatch mode: execute Diesel code from file or STDIN\n" +
	    "\t\tsimEngine [options] -batch <file.dsl>\n" +
	    "\t\tsimEngine [options] -batch\n\n" +
	    "\tInteractive mode: run simEngine as an interactive environment\n" +
	    "\t\tsimEngine [options]\n\n"+
	    "Currently available options include:\n\n" +
	    "-start <n>" +
	    "-stop <n>" +
	    "-inputs <file>" +
	    "-states <file>" +
	    "-outputs <file>" +
	    "-cpu" +
	    "-parallelcpu" +
	    "-gpu" +
	    "-double" +
	    "-float" +
	    "-single" +
	    "-help")
  end

  function getCommandLine() = LF getCommandLine()

  function parseOptions(options)
    var options_list = options
    var options_table = {}
    var option_name

    function getOptionValue()
      var option_value
      if options_list.length() == 0 then
	error("Missing value for option '" + option_name + "'.")
      else
	option_value = options_list.first()
	options_list = options_list.rest()
	if "-" == option_value.first() then
	error("Missing value for option '" + option_name + "'.")
	end
      end
      option_value
    end

    function getOptionValueNumber()
      var option_value
      if options_list.length() == 0 then
	error("Missing value for option '" + option_name + "'.")
      else
	option_value = options_list.first().tonumber()
	options_list = options_list.rest()
	if () == option_value then
	  error("Option value for '" + option_name + "' must be a number.")
	else
	  option_value
	end
      end
    end

    function addOption(name, value)
      if exists key in options_table.keys suchthat key == name then
	error("Conflicting option '" + option_name + "' for previously set " + name + ".")
      end
      options_table.add(name, value)
    end

    while(options_list.length() > 0) do
      option_name = options_list.first()
      options_list = options_list.rest()

      // Model file for compilation
      if "-model" == option_name then
	addOption("modelfile", getOptionValue())

      elseif "-start" == option_name then
	addOption("start", getOptionValueNumber())

      elseif "-stop" == option_name then
	addOption("stop", getOptionValueNumber())

      elseif "-instances" == option_name then
	addOption("instances", getOptionValueNumber())

      elseif "-inputs" == option_name then
	addOption("inputs", getOptionValue())

      elseif "-states" == option_name then
	addOption("states", getOptionValue())

      elseif "-outputs" == option_name then
	addOption("outputs", getOptionValue())

      elseif "-cpu" == option_name then
	addOption("target", "cpu")

      elseif "-parallelcpu" == option_name then
	addOption("target", "openmp")

      elseif "-gpu" == option_name then
	addOption("target", "cuda")

      elseif "-float" == option_name then
	addOption("precision", "float")

      elseif "-single" == option_name then
	addOption("precision", "float")

      elseif "-double" == option_name then
	addOption("precision", "double")

      elseif "-help" == option_name then
	addOption("help", true)

      elseif "-gnuplot" == option_name then
	addOption("gnuplot", true)

// How do we make these options unavailabe for release?
      elseif "-debug" == option_name then
	addOption("debug", true)

      elseif "-emulate" == option_name then
	addOption("emulate", true)

      elseif "-profile" == option_name then
	addOption("profile", true)

      elseif "-nocompile" == option_name then
	addOption("nocompile", true)
// ****************************************************

      else
	error("Unrecognized option " + option_name)
      end
    end
    options_table
  end
end
