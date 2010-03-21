var systemDependencies

function startup ()
  systemDependencies = Dependency.getDependencies()
  Devices.init()

  // Display start up message
  if settings.general.startupmessage.getValue() then
      println(LF sys_startupMessage())
  end

  //Simex.runModel()
  Profile.time ("Running Model", runModel, ())
end
