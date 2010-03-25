var systemDependencies

function startup ()
  systemDependencies = Dependency.getDependencies()
  Devices.init()

  // Display start up message
  if settings.general.startupmessage.getValue() then
      println(LF sys_startupMessage())
  end

  // If we're going to display the options, do it now
  if settings.logging.logsettings.getValue() then
    LF logSettings()
  end

  //Simex.runModel()
  profileTime ("Running Model", runModel, ())
end
