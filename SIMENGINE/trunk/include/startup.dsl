var systemDependencies

function startup ()
  systemDependencies = Dependency.getDependencies()
  Devices.init()

  //Simex.runModel()
  runModel()
end
