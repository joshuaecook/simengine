/* Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
 * For more information, please visit http://simatratechnologies.com/
 */

import "command_line.dsl"

// FIXME restore this namespace
//namespace Simex
  /* Executes a command in a shell.
   * Enables a sequence of processes connected by pipes to be
   * run in a single subprocess. */
  function shell (command: String, argv: Vector of String)
    var p = Process.run(command, argv)
    var out = Process.read(p)
    Process.reap(p)
    out
  end

  overload function shell (command: String)
    shell (command, [])
  end

  var arch64 = Sys.architecture.contains("64")

  class Make
    var CC = "gcc"
    var LD = "gcc"
    var CFLAGS = []
    var CPPFLAGS = []
    var LDFLAGS = []
    var LDLIBS = []
    var TARGET_ARCH = ["-m32"]
    var LD_TARGET_ARCH = ["-m32"]

    /* Returns a tuple of (compiler, options)
     * suitable for application by Process.run(). */
    function configureCompile (exfile: String, args)
      (CC, TARGET_ARCH + ["-o", exfile] + CFLAGS + CPPFLAGS + LDFLAGS + args + LDLIBS)
    end

  end

  /* A target-specific Make configuration.
   * A derived class shall exist for each supported target backend. */
  class Target
    var debug = false
    var profile = false
    var precision = "double"
    var parallelModels = 1
    var cFlags = ["-W", "-Wall", "-fPIC"]
    var cppFlags = []
    var ldFlags = []
    var ldLibs = ["-lm"]

    constructor(compilerSettings)
      debug = settings.simulation_debug.debug.getValue()
      profile = settings.simulation_debug.profile.getValue()
      precision = settings.simulation.precision.getValue()
    end

    function getOsLower() = shell("uname",["-s"])[1].rstrip("\n").translate("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz")

    function make ()
      var simEngine = Environment.getVar("SIMENGINE")
      var m = Make.new()
      var osLower = getOsLower()

      m.CFLAGS = cFlags.clone ()
      m.CPPFLAGS = cppFlags.clone ()
      m.LDFLAGS = ldFlags.clone ()
      m.LDLIBS = ldLibs.clone ()

      m.CPPFLAGS.push_front("-DPARALLEL_MODELS=" + (parallelModels.tostring()))

      if "double" <> precision then
        m.CPPFLAGS.push_back("-DSIMENGINE_STORAGE_float")
        m.CFLAGS.push_back("-I" + simEngine + "/include/float")
      else
        m.CPPFLAGS.push_back("-DSIMENGINE_STORAGE_double")
        m.CFLAGS.push_back("-I" + simEngine + "/include/double")
      end

      m.LDFLAGS.push_back("-L" + simEngine + "/lib")

      if "darwin" == osLower then
	m.CC = "gcc-4.2"
	m.LD = "gcc-4.2"
      end

      if arch64 then
        m.TARGET_ARCH = ["-m64"]
        m.LD_TARGET_ARCH = ["-m64"]
      end
      if "darwin" == osLower then
        m.TARGET_ARCH = ["-arch", "x86_64"]
        // mlton built for i386 on Mac and simlib needs to call into *.sim.  no need for universal binary
        m.LD_TARGET_ARCH = ["-arch", "i386"]
      end

      if debug then
        m.CFLAGS.push_back("-g")
        m.CFLAGS.push_back("-gdwarf-2")
      else
        m.CFLAGS.push_back("-DNDEBUG")
        m.CFLAGS.push_back("-O2")
        m.CFLAGS.push_back("-fno-strict-aliasing")
      end

      if profile then
        m.CFLAGS.push_back("-pg")
      end

      // Defers to the child class to finish the configuration.
      setupMake(m)

      m
    end

    function compile (outfile: String, args)
      var m = make ()
      m.configureCompile(outfile, args)
    end

  end

  class TargetCPU extends Target
    constructor(compilerSettings)
      super(compilerSettings)
    end
    function setupMake (m: Make)
      if "double" <> precision then
	m.LDLIBS.push_back("-lcvode_float")
      else
	m.LDLIBS.push_back("-lcvode_double")
      end

      m.CPPFLAGS.push_back("-DTARGET_CPU")
    end
  end

  class TargetOpenMP extends Target
    constructor(compilerSettings)
      super (compilerSettings)
      parallelModels = Devices.OPENMP.numProcessors()
      settings.simulation.parallel_models.setValue(parallelModels)
    end

    function setupMake (m: Make)
      m.CFLAGS.push_back("-fopenmp")
      m.LDLIBS.push_back("-lgomp")
      if "double" <> precision then
	m.LDLIBS.push_back("-lcvode_float")
      else
	m.LDLIBS.push_back("-lcvode_double")
      end

      m.CPPFLAGS.push_back("-DTARGET_OPENMP")
    end
  end

  class TargetCUDA extends Target
    var nvcc
    var emulate = false
    var gpuid = 1
    var cudaInstallPath
    var ptxasFlags = ["-v"]
    var openccFlags = ["-v"]//, "-OPT:Olimit=99999"]

    constructor(compilerSettings)
      super (compilerSettings)


      var deviceid = settings.gpu.gpuid.getValue()
      if deviceid == 9999 then
	gpuid = 1
      else
	foreach id in 1:(Devices.CUDA.numDevices) do
	  if deviceid == (Devices.CUDA.getProp(id, "deviceId").tonumber()) then
	    gpuid = id
	  end
	end
      end
      if gpuid < 1 or gpuid > Devices.CUDA.numDevices then
	nostack_error "Invalid GPU device id."
      end

      // MP * warp size * 4 to keep high residency (probably needs tweaking)
      parallelModels = Devices.CUDA.getProp(gpuid, "multiProcessorCount").tonumber() * 32 * 4
      if 2 == Devices.CUDA.getProp(gpuid, "major").tonumber() then
	// Fermi multiprocessors have 4x the number of cores vs. previous devices
	parallelModels = 4 * parallelModels
      end
      settings.simulation.parallel_models.setValue(parallelModels)

      precision = settings.simulation.precision.getValue()
      emulate = settings.simulation_debug.emulate.getValue()

      var deps = Dependency.getDependencies()

      nvcc = deps.nvcc.fullpath
      cudaInstallPath = Path.dir (Path.dir (nvcc))
      if Devices.CUDA.numDevices == 0 then
        nostack_error("Cannot target the GPU : " + Devices.CUDA.cudaErr)
      end
      var device_id = Devices.CUDA.getProp(gpuid, "deviceId")
      var device_arch = Devices.CUDA.getProp(gpuid, "arch")
      if 2 == Devices.CUDA.getProp(gpuid, "major").tonumber() then
      	device_arch = "sm_13"
      end
      if not emulate and precision == "double" and (device_arch == "sm_10" or device_arch == "sm_11" or device_arch == "sm_12") then
        warning("CUDA device does not support double precision. Defaulting to single precision float.")
        precision = "float"
        settings.simulation.precision.setValue("float")
      end
    end

    function setupMake (m: Make)
      var osLower = getOsLower()

      m.CC = nvcc
      m.CPPFLAGS.push_back("-DTARGET_GPU")
      m.CFLAGS.push_front("-I" + cudaInstallPath + "/include")
      m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib")

      m.TARGET_ARCH = ["-m32"]

      if osLower == "darwin" then
        m.TARGET_ARCH = ["-arch", "i386"] // nVidia tools don't support 64bit on Darwin
        m.LD = "g++-4.2"
      elseif arch64 then
	m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib64")
      end

      // nvcc and gcc have different meanings for ARCH so set them specifically
      // as part of CFLAGS and remove the TARGET_ARCH value
      m.CFLAGS = m.TARGET_ARCH + m.CFLAGS
      m.TARGET_ARCH = []

      // Wrap all gcc flags in --compiler-options when passed to nvcc
      m.CFLAGS = ["--host-compilation", "c",
		  "--device-compilation", "c",
		  "--compiler-options", join(" ", m.CFLAGS),
		  "--ptxas-options", join(" ", ptxasFlags),
		  "--opencc-options", join(" ", openccFlags)]
      
      // Currently we use only the first device returned from device_props program
      // which returns a list of available devices sorted by their GFLOPs
      var device_id = Devices.CUDA.getProp(gpuid, "deviceId")
      var device_arch = Devices.CUDA.getProp(gpuid, "arch")
      if 2 == Devices.CUDA.getProp(gpuid, "major").tonumber() then
      	device_arch = "sm_13"
      end
      m.CFLAGS.push_back("-DSIMENGINE_CUDA_DEVICE=" + device_id)
      m.CFLAGS.push_front("-arch=" + device_arch)

      /* Does device debugging really work? I get strange errors from cuda-gdb. */
      if debug then
        m.CFLAGS = ["-g", "-G"] + m.CFLAGS
      end

      if emulate then
	m.CFLAGS.push_front("-deviceemu")
	m.CPPFLAGS.push_front("-D__DEVICE_EMULATION__")
      end

      m.LDLIBS.push_back("-lcudart")
    end
  end

  var targetOptions = {cpu = "cpu",
		       parallelcpu = "openmp",
		       gpu = "cuda"}

  var precisionOptions = {float = "float",
			  single = "float",
			  double = "double"}

  var booleanOptionNamesAlways = ["help",
				  "binary",
				  "interface",
                                  "shared_memory"] +
				  targetOptions.keys +
				  precisionOptions.keys

  var booleanOptionNamesDebug = ["debug",
				 "emulate",
				 "profile",
				 "nocompile"]

  var numberOptionNamesAlways = ["instances",
				 "start",
				 "stop",
				 "seed",
				 "buffer_count",
				 "max_iterations",
				 "gpu_block_size",
				 "all_timesteps"]

  var numberOptionNamesDebug = ["parallelmodels",
				"gpuid"]

  var stringOptionNamesAlways = ["simex",
				 "inputs",
                                 "outputdir",
                                 "json_interface",
				 "target",
				 "precision"]
  var stringOptionNamesDebug = []

  function defaultCompilerSettings() = {target = settings.simulation.target.getValue(),
					precision = settings.simulation.precision.getValue(),
					parallel_models = settings.simulation.parallel_models.getValue(),
					debug = settings.simulation_debug.debug.getValue(), 
					emulate = settings.simulation_debug.emulate.getValue(),
					profile = settings.simulation_debug.profile.getValue(),
					outputdir = settings.simulation.outputdir.getValue()}

  // The following parameters are parsed by simEngine but then passed along to the simulation executable
  var simulationSettingNames = ["start", "stop", "instances", "inputs", "outputdir", "binary", "seed", "gpuid", "shared_memory", "buffer_count", "max_iterations", "gpu_block_size", "all_timesteps"]
  function defaultSimulationSettings() = {start = 0,
					  instances = 1,
					  outputdir = settings.compiler.outputdir.getValue()}


  function runModel()
    
    var booleanOptionNames = booleanOptionNamesAlways
    var numberOptionNames = numberOptionNamesAlways
    var stringOptionNames = stringOptionNamesAlways

    // FIXME, check a DOL option for whether the debug options should be allowed
    var DEBUG = 1
    if DEBUG == 1 then
      booleanOptionNames = booleanOptionNames + booleanOptionNamesDebug
      numberOptionNames = numberOptionNames + numberOptionNamesDebug
      stringOptionNames = stringOptionNames + stringOptionNamesDebug
    end

    var commandLineSettings = CommandLine.parseCommandLine(booleanOptionNames, numberOptionNames, stringOptionNames)

    var simexFile = settings.general.simex.getValue()
    var compileFile = settings.general.compile.getValue()
    var simulateFile = settings.general.simulate.getValue()
    // verify mutual exclusion
    if ((simexFile <> "" and compileFile <> "") or
	(simexFile <> "" and simulateFile <> "") or
	(compileFile <> "" and  simulateFile <> "")) then
	nostack_error("Only one option of -simex, -compile, and -simulate is supported at one time")
    end

    if settings.general.help.getValue() then
      printUsage(LF settingsHelp())
    elseif simexFile <> "" then
      var modelFile = FileSystem.realpath(simexFile)
      // If a model was passed as a parameter, compile and/or execute it
      if () <> modelFile then
	var compilerSettings = validateCompilerSettings(commandLineSettings)
	var simulationSettings = validateSimulationSettings(commandLineSettings)
	var exfile = profileTime ("autoRecompile", autoRecompile, (modelFile, compilerSettings))

	LF sys_collect_and_pack ()

	if () <> simulationSettings and "" <> exfile then
	  var simulation = FileSystem.realpath(exfile)
	  profileTime ("simulating", simulate, (simulation, simulationSettings, settings.simulation_debug.debug.getValue()))
	else
	  //println("Not running: " + compilerSettings.exfile)
	end
      else
	nostack_error("The model file '" + simexFile + "' does not exist.")
      end
    elseif compileFile <> "" then
      var modelFile = FileSystem.realpath(compileFile)
      // If a model was passed as a parameter, compile and/or execute it
      if () <> modelFile then
	var compilerSettings = validateCompilerSettings(commandLineSettings)
	var simulationSettings = validateSimulationSettings(commandLineSettings)
	var exfile = autoRecompile(modelFile, compilerSettings)

	LF sys_collect_and_pack ()

      else
	nostack_error("The model file '" + compileFile + "' does not exist.")
      end
    elseif simulateFile <> "" then
      var simFile = FileSystem.realpath(simulateFile)
      if () <> simFile then
	// TODO: add support for this mode
	nostack_error("The simulate flag is not currently supported by simEngine")
      else
	nostack_error("The simulation file '" + simulateFile + "' does not exist")
      end
    else
      // don't do anything..
    end

  end

  function simulate(simulation, simulationSettings, debug)
    var simexCommands = []
    var gdb = objectContains(settings.simulation_debug, "gdb") and settings.simulation_debug.gdb.getValue()
    var p

    foreach setting in simulationSettings.keys do
      // C command-line options use "--" instead of "-" (artifact of getopt library)
      simexCommands.push_back("--" + setting)
      if simulationSettings.getValue(setting) <> true then
	simexCommands.push_back(simulationSettings.getValue(setting).tostring())
      end
    end
    if gdb then
      var emacsArgs
      if "gpu" == settings.simulation.target.getValue() then
	emacsArgs = ["--eval", "(debugit \"" + settings.compiler.outputdir.getValue() + "\" \"" + simulation + "\" \"" + join(" ", simexCommands) + "\" \"cuda-gdb\")"]
      else
	emacsArgs = ["--eval", "(debugit \"" + settings.compiler.outputdir.getValue() + "\" \"" + simulation + "\" \"" + join(" ", simexCommands) + "\")"]
      end
      println("Launching simulation in gdb: emacsclient '" + join("' '", emacsArgs) + "'")
      // emacsclient does not return until the file opened is closed with Ctrl-X #
      p = Process.run("emacsclient", ["emacsclient-buffer_close_to_allow_matlab_to_continue"])
      // emacsclient returns immediately when called with '--eval'
      Process.run("emacsclient", emacsArgs)
    else
      if debug then
	println("Run: " + simulation + " " + (join(" ", simexCommands)))
      end
      p = Process.run(simulation, simexCommands)
    end
    var allout = Process.readAll(p)
    var stat = Process.reap(p)
    var stdout = allout(1)
    var stderr = allout(2)

    if (1 == stat) then
      nostack_error(join("",stderr))
    elseif (() == stat or 1 < stat) then
      failure("Unexpected internal exception was generated during simulation: " + join("",stderr))
      // Return the interface from the executable if requested
    elseif objectContains(simulationSettings, "interface") then
      print(join("", stdout))
    end
  end

  function printUsage(settingshelp)
    println("\nsimEngine usage:\n\n" +
	    "\tSimulation mode: run a simulation from a Diesel model\n" +
	    "\t\tsimEngine [options] --simex <modelfile.dsl>\n\n" +
	    "\tBatch mode: execute Diesel code from file or STDIN\n" +
	    "\t\tsimEngine [options] --batch <file.dsl>\n" +
	    "\t\tsimEngine [options] --batch -\n\n" +
	    "\tInteractive mode: run simEngine as an interactive environment\n" +
	    "\t\tsimEngine [options]\n\n"+
	    "Currently available options include:\n\n" +
	    settingshelp + "\n" +
	    "For further information, please visit us at www.simatratechnologies.com.\n")
    // FIXME: automatically add the available options from the actual options above
  end

  function getModelFile(commandLineOptions: Table)
    var simex = settings.general.simex.getValue()

    // Set the full realpath of the model file
    if simex=="" then
      ()
    else
      var modelfilepath = FileSystem.realpath(simex)
      if () == modelfilepath then
	nostack_error("The model file '" + simex + "' does not exist.")
      end
      modelfilepath
    end
  end

  function validateCompilerSettings(commandLineOptions: Table)
    var compilerSettings = {}

    var systemDependencies = Dependency.getDependencies()

    var usingGPU = "gpu" == settings.simulation.target.getValue()
    if usingGPU and not (Dependency.checkGPUDependencies(systemDependencies)) then
	println("Rechecking dependencies ... ")
        systemDependencies = Dependency.buildDependencyFile()
	if not(Dependency.checkGPUDependencies(systemDependencies)) then
	  nostack_error "Not all dependencies have been met for using the GPU, simEngine cannot continue with GPU compilation."
	end
    end

    // TODO: Remove all occurances of emulate - deprecated by nVidia
    //if settings.simulation_debug.emulate.getValue() then
	//if usingGPU then
	    // this is ok
	//else
	    //nostack_error("Emulation is only valid for the GPU.")
	//end
    //end

    if usingGPU then
      var deviceid = settings.gpu.gpuid.getValue()
      if deviceid == 9999 then
        settings.gpu.gpuid.setValue(Devices.CUDA.getProp(1, "deviceId").tonumber())
      end
    end

    // Set up the name of the c source file
    var name = Path.base(Path.file (settings.general.simex.getValue()))
    var cname = ""
    if usingGPU then
      cname = name + ".cu"
    else
      cname = name + ".c"
    end
    settings.compiler.cSourceFilename.setValue(cname)

    compilerSettings
  end

  function validateSimulationSettings(commandLineOptions: Table)
    var simulationSettings = {}

    function copyOptions(tableSource, tableDest, options)
      var copied = false
      foreach option in options do
	if objectContains(tableSource, option) then
	  tableDest.add(option, tableSource.getValue(option))
	  copied = true
	end
      end
      copied
    end

    function createSimulationTable (tableDest)	
        if objectContains(settings.simulation, "inputs") then
	    tableDest.add("inputs", join(":", settings.simulation.inputs.getValue()))
        end
	if objectContains(settings.simulation, "start") then
	    tableDest.add("start", settings.simulation.start.getValue())
	end	
	if objectContains(settings.simulation, "stop") then
	    tableDest.add("stop", settings.simulation.stop.getValue())
	end	
	tableDest.add("instances", settings.simulation.instances.getValue())
	tableDest.add("outputdir", settings.compiler.outputdir.getValue())
	tableDest.add("binary", settings.simulation.binary.getValue())
	if objectContains(settings.simulation, "shared_memory") then
          tableDest.add("shared_memory", settings.simulation.shared_memory.getValue())
	end
	if objectContains(settings.simulation, "buffer_count") then
	  tableDest.add("buffer_count", settings.simulation.buffer_count.getValue())
	end
	// HACK BEGIN
	if objectContains(settings.simulation, "all_timesteps") then
	  tableDest.add("all_timesteps", settings.simulation.all_timesteps.getValue())
	end
	// HACK END
	if "gpu" == settings.simulation.target.getValue() then
	    tableDest.add("gpuid", settings.gpu.gpuid.getValue())
	    if objectContains(settings.gpu, "max_iterations") and settings.gpu.max_iterations.getValue() > 0 then
	      tableDest.add("max_iterations", settings.gpu.max_iterations.getValue())
	    end
	    if objectContains(settings.gpu, "gpu_block_size") and settings.gpu.gpu_block_size.getValue() > 0 then
	      tableDest.add("gpu_block_size", settings.gpu.gpu_block_size.getValue())
	    end
	end
	if objectContains(settings.simulation, "seed") then
	    tableDest.add("seed", settings.simulation.seed.getValue())
	end	
	true
    end

    // Check to see if the interface is requested
    if objectContains(commandLineOptions, "interface") then
      simulationSettings.add("interface", true)
    elseif objectContains(settings.compiler, "json_interface") then
      simulationSettings.add("json_interface", settings.compiler.json_interface.getValue())
    end

    // Set all the simulation settings from the commandLineOptions
    if createSimulationTable(simulationSettings) then
      if not(objectContains(simulationSettings, "stop")) then
	// If any simulation options were set but no stop time or interface request was, tell the user this doesn't make sense
	nostack_error("In order to simulate a stop time must be specified.")
      else

	// Check if user specified a start time but no stop time
	if objectContains(simulationSettings, "start") and not(objectContains(simulationSettings, "stop")) then
	  nostack_error("Specifying a start time has no meaning without a corresponding stop time.")
	end

	// Add any defaults for settings that were not set from the command-line
	foreach key in defaultSimulationSettings().keys do
	  if not(objectContains(simulationSettings, key)) then
	    simulationSettings.add(key, defaultSimulationSettings().getValue(key))
	  end
	end

	// Check if user specified a stop time less than the start time
	if simulationSettings.stop <> 0 and simulationSettings.stop <= simulationSettings.start then
	  nostack_error("Stop time "+simulationSettings.stop+" must be greater than the start time "+simulationSettings.start+".")
	end

	// Check for a valid number of instances
	if simulationSettings.instances < 1 or simulationSettings.instances.floor() <> simulationSettings.instances then
	  nostack_error("Number of model instances must be an integer value of 1 or more.")
	end
      end
    end
    if simulationSettings.keys.length() > 0 then
      simulationSettings
    else
      ()
    end
  end

  // populateCompilerSettings - adds settings from the global settings structure into compiler settings
  // everything in here will be added to the generated archive manifest
  function populateCompilerSettings(compilerSettings)
    compilerSettings.add("target", settings.simulation.target.getValue())
    compilerSettings.add("precision", settings.simulation.precision.getValue())
    compilerSettings.add("optimize", settings.optimization.optimize.getValue())
    compilerSettings.add("aggregate", settings.optimization.aggregate.getValue())
    compilerSettings.add("flatten", settings.optimization.flatten.getValue())
    compilerSettings.add("debug", settings.simulation_debug.debug.getValue())
    compilerSettings.add("profile", settings.simulation_debug.profile.getValue())
    compilerSettings.add("emulate", settings.simulation_debug.emulate.getValue())
  end

  function autoRecompile (filename: String, compilerSettings: Table)
    if "dsl" <> Path.ext(filename) then
      nostack_error ("Unknown type of file " + filename)
    end
    var working_dir = FileSystem.pwd ()
    var dir = Path.dir filename
    var file = Path.file filename
    var simfile = Path.join(working_dir, ((Path.base file) + ".sim"))

    var needsToCompile = true
    var target

    // Instantiating a target could override compilerSettings based on target type (i.e. precision = float for GPU arch_11)
    var target_setting = settings.simulation.target.getValue()

    // if the default, then we choose one based on the license
    if "default" == target_setting then
      target_setting = LF defaultTarget()
    end

    if "parallelcpu" == target_setting then
      target = TargetOpenMP.new(compilerSettings)
    elseif "gpu" == target_setting then
      target = TargetCUDA.new(compilerSettings)
    elseif "cpu" == target_setting then
      target = TargetCPU.new(compilerSettings)
    else
      nostack_error ("Unknown target " +target_setting)
    end

    // Make sure the outputs directory exists, may be created before simEngine is called
    // otherwise, create it
    var outputDir = settings.compiler.outputdir.getValue()
    var prevDir = FileSystem.pwd()

    if not(FileSystem.isdir(outputDir)) then
      FileSystem.mkdir(outputDir)
      if not(FileSystem.isdir(outputDir)) then
	nostack_error("Could not create directory " + outputDir)
      end
    end

    // Change working directory to outputdir directory, store the previous dir
    FileSystem.chdir(outputDir)

    // Opening an archive succeeds if the file exists and a manifest can be read.
    var archive = Archive.openArchive (simfile)
    // Check to make sure that regenerateTimings is not set
    if settings.logging.regenerateTimings.getValue() == false then
      if () <> archive then
	// .sim must be newer than the simEngine compiler
	var creation = Archive.creationDate (archive)
	if creation > Sys.buildTime then
	  // .sim must be using the same DOL file and be younger than it
	  var dol = Archive.dolFilename (archive)
	  if FileSystem.isfile(dol) and creation > FileSystem.modtime (dol) and dol == settings.compiler.registry.value then
	    // TODO check environment and version of SIM

	    // .sim must be younger than each imported DSL file
	    var dsls = Archive.dslFilenames (archive)
	    var main = dsls.first () // an absolute path
	    var imports = dsls.rest () // paths relative to main
	    
	    if filename == main then
	      var upToDate = creation > FileSystem.modtime (main)
	      foreach i in imports do
		upToDate = (upToDate and 
			    FileSystem.isfile i and
			    creation > FileSystem.modtime i)
		if FileSystem.isfile i then 
		  if creation < FileSystem.modtime i then
		    notice ("Source file <"+i+"> is not up to date")
		  end
		else
		  notice ("Source file <"+i+"> no longer exists")
		end
	      end
	    
	      if upToDate then
		needsToCompile = not (isArchiveCompatibleWithCompilerSettings (archive, compilerSettings))
		if needsToCompile then
		  notice ("Precompiled SIM file is not compatible with compiler settings")
		end
	      end	    
	    else
	      notice ("Filename <"+filename+"> is not the same as the main function <"+main+">")			
	    end
	  else
	    notice ("DOL registry file is not up to date")
	  end
	else
	  notice ("Build time is more recent than the SIM file creation date")
	end
      else
	notice ("No SIM file has been found, must be compiled")
      end
    else
      notice ("Always recompiling when regenerateTimings flag is set to true")
    end

    var exfile

    if needsToCompile then
      populateCompilerSettings(compilerSettings)
      var success = compile (filename, target, compilerSettings, simfile)
      if success <> 0 then
	LF sys_exit(128)
	exfile = ""
      end
      exfile = Path.join(settings.compiler.outputdir.getValue(), compilerSettings.exfile)
    else
      exfile = Path.join(settings.compiler.outputdir.getValue(), compilerSettings.exfile)
      if settings.simulation_debug.debug.getValue() then
	println ("Reusing existing executable from sim file :" + exfile)
      end
    end

    // Restore the previous working directory
    FileSystem.chdir(prevDir)

    exfile
  end

  function isArchiveCompatibleWithCompilerSettings (archive, compilerSettings)
    function compatible (executable)
      var target_setting = settings.simulation.target.getValue()
      var precision_setting = settings.simulation.precision.getValue()
      var debug_setting = settings.simulation_debug.debug.getValue()
      var profile_setting = settings.simulation_debug.profile.getValue()
      var emulate_setting = settings.simulation_debug.emulate.getValue()
      var optimize_setting = settings.optimization.optimize.getValue()
      var aggregate_setting = settings.optimization.aggregate.getValue()
      var flatten_setting = settings.optimization.flatten.getValue()

      if executable.target <> target_setting then
	  notice ("Target setting '"+target_setting+"' is not equal to the archive setting '"+executable.target+"'")
      end
      if executable.precision <> precision_setting then
	  notice ("Precision setting '"+precision_setting+"' is not equal to the archive setting '"+executable.precision+"'")
      end
      if executable.debug <> debug_setting then
	  notice ("Debug setting '"+debug_setting+"' is not equal to the archive setting '"+executable.debug+"'")
      end
      if executable.profile <> profile_setting then
	  notice ("Profile setting '"+profile_setting+"' is not equal to the archive setting '"+executable.profile+"'")
      end
      if executable.emulate <> emulate_setting then
	  notice ("Emulate setting '"+emulate_setting+"' is not equal to the archive setting '"+executable.emulate+"'")
      end

      var compat = ((executable.target == target_setting) and
		    (executable.precision == precision_setting) and
		    (executable.debug or not(debug_setting)) and
		    (executable.profile == profile_setting) and
		    (executable.optimize == optimize_setting) and
		    (executable.aggregate == aggregate_setting) and
		    (executable.flatten == flatten_setting) and
		    (not("gpu" == executable.target)
		     or executable.emulate == emulate_setting))
      if compat then
	  compilerSettings.add("exfile", executable.exfile)
      end
      compat
    end

    () <> Archive.findExecutable (archive, compatible)
  end

  function compile (filename, target, compilerSettings, simfile)
    if settings.simulation_debug.debug.getValue() then
      println ("Compiling " + filename)
    end

    var name
    var stat
    var imports

    if settings.compiler.fastcompile.getValue() then
	stat = LF compile (filename)
	var paths = LF str_split (filename, "/")
	var filenameparts = LF str_split (paths.at(paths.length()), ".")
	name = filenameparts.first()
	imports = [filename]
    else
	var mod = LF loadModel (filename)
	name = mod.template.name
	mod.template.settings = compilerSettings
	imports = mod.template.imports
	var instantiatedModel = mod.instantiate()
	stat = LF compile (instantiatedModel)
    end

    LF sys_collect_and_pack ()
    
    if 0 == stat then
      profileTime ("Create SIM File", Archive.createArchive, (simfile, settings.compiler.registry.value, imports, target, compilerSettings))
      println("Compilation completed successfully")
    elseif 3 >= stat then
	// Show the error code
	if 1  == stat then
	    println("Error encountered in translation of DSL model")
	elseif 2 == stat then
	    println("Error encountered in compilation of DSL model")
	elseif 3 == stat then
	    println("Error encountered in code generation of DSL model")
	end
    elseif 6 >= stat then
	if 4 == stat then
	    failure("Unexpected internal exception was generated during translation")
	elseif 5 == stat then
	    failure("Unexpected internal exception was generated during compilation")
	elseif 6 == stat then
	    failure("Unexpected internal exception was generated during code generation")
	end
    elseif 7 == stat then
	failure("Unexpected DSL failure was generated during compilation")
    end

    stat
  end
//end


