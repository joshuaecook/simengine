namespace CommandLine
  function parseCommandLine(booleanOptionNames, numberOptionNames, stringOptionNames)
    parseOptions(getCommandLine(), booleanOptionNames, numberOptionNames, stringOptionNames)
  end

  function getCommandLine() = LF getCommandLine()

  function parseOptions(options, booleanOptionNames, numberOptionNames, stringOptionNames)
    var optionsList = options // local copy of options that will be successively removed as parsed
    var optionsTable = {} // return value table of key/value option pairs
    var optionName // currently processed option name

    // Ensure that an option that requires a string value has a string value (not another option name)
    // removes value from the optionsList
    function getOptionValueString()
      var optionValue
      if optionsList.length() == 0 then
	error("Missing value for option '" + optionName + "'.")
      else
	optionValue = optionsList.first()
	optionsList = optionsList.rest()
	if "-" == optionValue.first() then
	error("Missing value for option '" + optionName + "'.")
	end
      end
      optionValue
    end

    // Ensure that an option that requires a number, has a value that is a number
    // removes value from the optionsList
    function getOptionValueNumber()
      var optionValue
      if optionsList.length() == 0 then
	error("Missing value for option '" + optionName + "'.")
      else
	optionValue = optionsList.first().tonumber()
	optionsList = optionsList.rest()
	if () == optionValue then
	  error("Option value for '" + optionName + "' must be a number.")
	else
	  optionValue
	end
      end
    end

    // Ensure that an option is only specified once and add it to the parsed options
    function addOption(name, value)
      if exists key in optionsTable.keys suchthat key == name then
	error("Option '" + optionName + "' may only be specified once.")
      end
      optionsTable.add(name, value)
    end

    while(optionsList.length() > 0) do
      optionName = optionsList.first()
      optionsList = optionsList.rest()

      if optionName.first() <> "-" then
	error("Options must be preceeded with a '-'.  Invalid option '" + optionName + "'.")
      end

      // Strip the '-' for comparison
      optionName = optionName.rest()

      if exists name in booleanOptionNames suchthat name == optionName then
	addOption(optionName, true)
      elseif exists name in numberOptionNames suchthat name == optionName then
	addOption(optionName, getOptionValueNumber())
      elseif exists name in stringOptionNames suchthat name == optionName then
	addOption(optionName, getOptionValueString())
      else
	error("Unrecognized option '-" + optionName + "'")
      end
    end
    optionsTable
  end
end
