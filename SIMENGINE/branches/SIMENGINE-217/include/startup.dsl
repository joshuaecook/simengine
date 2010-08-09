var systemDependencies

function startup ()
  systemDependencies = Dependency.getDependencies()
  Devices.init()

  // Display start up message
  if settings.general.startupmessage.getValue() then
      println(LF sys_startupMessage())
      // Now check to see if we are up to date
      var update_info = LF validateUpdate (settings.installation.updateBuildDate.getValue())
      var buildNum = settings.installation.updateBuildNumber.getValue()
      var new_version = false
      var dev_version = false
      if 1 == update_info.order then
	  new_version = true
      elseif 0 == update_info.order then // check the build number
	  var sysBuildNum = LF sys_build_number()
	  dev_version = buildNum > 0 // only set if we are looking at a development build
	  if buildNum > sysBuildNum then 
	      new_version = true
	  end
      end
      if new_version then // this means that there is a newer version online
	var ver_str = "" + settings.installation.updateMajorVersion.getValue() + "." + 
	settings.installation.updateMinorVersion.getValue() + settings.installation.updateRevision.getValue()
	var date_str = Time.daysToString(settings.installation.updateBuildDate.getValue())
	if dev_version then
	    if update_info.valid then
		println(" ")
		println("An updated development version, v" + ver_str + " (build #" + buildNum + "), of simEngine has been")
		println("released on " + date_str + ", and is now available for download.  Please ")
		println("execute simEngineUpdate in MATLAB to update your copy of simEngine to the latest version.")
		println(" ")
	    else // I think we can safely ignore this case - won't update dev builds if their license is not valid
		/*
		var expire_date = Licensing.licenseExpirationDate()
		println(" ")
		println("An updated version, v" + ver_str + ", of simEngine has been released on " + data_str + ".")
		println("Unfortunately, your maintenance subscription expired on " + expire_date + ".  Please")
		println("visit www.simatratechnologies.com to renew or contact us at")
		println("support@simatratechnologies.com for more information.")
		println(" ")
		 */
	    end
	else
	    if update_info.valid then
		println(" ")
		println("An updated version, v" + ver_str + ", of simEngine has been released on " + date_str + ",")
		println("and is now available for download.  Please execute simEngineUpdate in MATLAB ")
		println("to update your copy of simEngine to the latest version or download the latest copy")
		println("at www.simatratechnologies.com.")
		println(" ")
	    else
		var expire_date = Licensing.licenseExpirationDate()
		println(" ")
		println("An updated version, v" + ver_str + ", of simEngine has been released on " + data_str + ".")
		println("Unfortunately, your maintenance subscription expired on " + expire_date + ".  Please")
		println("visit www.simatratechnologies.com to renew or contact us at")
		println("support@simatratechnologies.com for more information.")
		println(" ")
	    end
	end
      end
  end

  // If we're going to display the options, do it now
  if settings.logging.logsettings.getValue() then
    LF logSettings()
  end

  //Simex.runModel()
  profileTime ("Running Model", runModel, ())
end
