// Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

namespace Archive

  constant VERSION = 0

  class Archive
    var dirty
    var filename
    var manifest
    var workingPath

    constructor (isDirty, aFilename, aWorkingPath, aManifest)
      dirty = isDirty
      filename = aFilename
      workingPath = aWorkingPath
      manifest = aManifest
    end
  end

  // Opens an existing archive with a given filename.
  // Returns () if a file with that name doesn't exist or
  // if the file is not a valid archive.
  function openArchive (filename)
    println("openArchive")
    if (FileSystem.isfile(filename)) then

      // Unzip the archive
      var uzp = Process.run("unzip", ["-o", filename])
      var allout = Process.readAll(uzp)
      var uzstat = Process.reap(uzp)
      var uzout = allout(1)
      var uzerr = allout(2)

      println("Unzip: unzip -o " + filename + "\nSTDOUT:\n" + join("", uzout) + "\n\nSTDERR:\n" + join("", uzerr))

      if (uzstat <> 0) then
	//FileSystem.rmfile(filename)
	warning("Failure reading manifest from " + filename + ".  A new SIM file will be generated.")
	()
      else
	var manifestFile = File.openTextIn("MANIFEST.json")
	var manifest = JSON.decode(manifestFile.getall())
	manifestFile.close()

	Archive.new (false, filename, "", manifest)
      end
    else
      println("Archive " + filename + " doesn't exist!")
      ()
    end
  end

  function createManifest (dolFilename, dslFilenames, environment, executables)
    {creationDate = Time.timestampInSeconds (),
     dolFilename = dolFilename,
     dslFilenames = dslFilenames,
     environment = environment,
     executables = executables,
     version = VERSION}
  end

  // Creates a new archive with a given filename. 
  // It is an error to attempt to create an archive if a file with that name already exists.
  function createArchive (filename, dolFilename, dslFilenames, target, compilerSettings)
    var environment = {FIXME="needs environment"}

    var cfile = settings.compiler.cSourceFilename.getValue()
    var exfile = (Path.base (Path.file cfile))
    compilerSettings.add("exfile", exfile)

    var manifest = createManifest (dolFilename, dslFilenames, environment, [compilerSettings])

    Archive.new (true, filename, "", manifest)

    var manifestData = JSON.encode manifest
    manifestData = JSON.addMember (manifestData, "license", LF licenseToJSON ())
    manifestData = JSON.addMember (manifestData, "settings", LF settingsToJSON ())

    var main = dslFilenames.first ()
    var imports = dslFilenames.rest ()

    var manifestFile = File.openTextOut("MANIFEST.json")
    manifestFile.putstr(manifestData)
    manifestFile.close()

    var cc = target.compile (exfile, [cfile])
    if settings.simulation_debug.debug.getValue() == true then
      println ("Compile: " + cc(1) + " '" + join("' '", cc(2)) + "'")
    end
    compile (cc(1), cc(2))

    // Create the archive using zip
    var zp = Process.run("zip", ["-r", filename, "."])
    var zallout = Process.readAll(zp)
    var zstat = Process.reap(zp)
    var zout = zallout(1)
    var zerr = zallout(2)

    println("Zip: zip -r " + filename + " .\nSTDOUT:\n" + join("", zout) + "\n\nSTDERR:\n" + join("", zerr))

    if zstat <> 0 then
      warning("Failure to close/create archive: " + filename)
    end
    ()
  end

  hidden function compile (cc, ccflags)
    var ccp = Process.run(cc,ccflags)
    var ccallout = Process.readAll(ccp)
    var ccstat = Process.reap(ccp)
    var ccout = ccallout(1)
    var ccerr = ccallout(2)
    if settings.logging.logexternal.getValue() then 
      println ("STDOUT:" + join("", ccout))
      println ("STDERR:" + join("", ccerr))
    end
    if 0 <> ccstat then
      failure ("Unexpected failure was encountered during generated code compilation.") //: " + join("", ccerr))
    end
  end

  function destroy (archive)
    if archive.dirty then
      // FIXME delete working files
    end
    FileSystem.rmfile (archive.filename)
  end

  // Returns the version id of the archive. 
  // Archives with differing versions are not ensured to be compatible.
  function version (archive) = archive.version
  // Returns the time at which the archive was created.
  function creationDate (archive) = archive.manifest.creationDate

  // Returns the pathname of the DOL settings file used when compiling.
  function dolFilename (archive) = archive.manifest.dolFilename

  // Returns the vector of pathnames for all files making up this model.
  // The first element will be an absolute path to the main DSL file.
  // Other elements are relative paths of imported files. 
  function dslFilenames (archive) = archive.manifest.dslFilenames

  // Applies a predicate function to each executable in the manifest,
  // until the predicate is satisfied or all executables have been
  // evaluated.
  // Returns the executable satisfying the predicate or 
  // () if the predicate could not be satisfied.
  function findExecutable (archive, predicate)
    function recur (executables)
      if executables.isempty () then ()
      else 
	var car = executables.first ()
	if predicate car then car
	else recur (executables.rest ())
	end
      end
    end
    recur (archive.manifest.executables)
  end
end
