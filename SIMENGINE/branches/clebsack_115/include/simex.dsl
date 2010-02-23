/* Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
 * For more information, please visit http://simatratechnologies.com/
 */

import "command_line.dsl"

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

    /* Returns a tuple of (compiler, options)
     * suitable for application by Process.run(). */
    function compile (exfile: String, args)
      (CC, TARGET_ARCH + ["-o", exfile] + CFLAGS + CPPFLAGS + LDFLAGS + args + LDLIBS)
    end

    /* Returns a tuple of (linker, options)
     * suitable for application by Process.run(). */
    function link (outfile: String, args)
      (LD, TARGET_ARCH + ["-o", outfile] + LDFLAGS + args)
    end
  end

  /* A target-specific Make configuration.
   * A derived class shall exist for each supported target backend. */
  class Target
    var debug = false
    var profile = false
    var precision = "double"
    var parallelModels = 1
    var cFlags = ["-W", "-Wall", "-fPIC", "-fopenmp"]
    var cppFlags = []
    var ldFlags = []
    var ldLibs = ["-ldl", "-lm", "-lgomp"]

    constructor(compilerSettings)
      debug = compilerSettings.debug
      profile = compilerSettings.profile
      precision = compilerSettings.precision
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
      end
      if "darwin" == osLower then
        m.TARGET_ARCH = ["-arch", "i386", "-arch", "x86_64"]
      end

      if debug then
        m.CFLAGS.push_back("-g")
        m.CFLAGS.push_back("-gdwarf-2")
      else
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
      m.compile(outfile, args)
    end

    function link (soname: String, outfile: String, args)
      var m = make()
      var osLower = getOsLower()

      if "darwin" <> osLower then
	m.LDFLAGS.push_back("-shared")
	m.LDFLAGS.push_back("-Wl,-soname,"+soname)
      else
	m.LDFLAGS.push_back("-dynamiclib")
	m.LDFLAGS.push_back("-Wl,-install_name,"+soname)
      end
      m.link(outfile, args)
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
      compilerSettings.parallel_models = parallelModels
    end

    function setupMake (m: Make)
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
    var cudaInstallPath
    var ptxasFlags = ["-v"]
    var openccFlags = ["-v"]//, "-OPT:Olimit=99999"]

    constructor(compilerSettings)
      super (compilerSettings)

      // MP * warp size * 4 to keep high residency (probably needs tweaking)
      parallelModels = Devices.CUDA.getProp(1, "multiProcessorCount").tonumber() * 32 * 4
      compilerSettings.parallel_models = parallelModels

      precision = compilerSettings.precision
      emulate = compilerSettings.emulate

      var cc = shell("which", ["nvcc"])
      if cc.isempty() then 
	error "Could not find nvcc. Please ensure that it exists in your path."
      end
      nvcc = FileSystem.realpath (cc[1].rstrip("\n"))
      cudaInstallPath = Path.dir (Path.dir (nvcc))
      if Devices.CUDA.numDevices == 0 then
        error("Cannot target the GPU : " + Devices.CUDA.cudaErr)
      end
      // TODO: This check may need to be expanded as more devices/architectures appear (e.g. no devices currently of arch sm_12)
      var device_id = Devices.CUDA.getProp(1, "deviceId")
      var device_arch = Devices.CUDA.getProp(1, "arch")
      if not emulate and precision == "double" and device_arch <> "sm_13" then
        warning("CUDA device does not support double precision. Defaulting to single precision float.")
        precision = "float"
        compilerSettings.precision = "float"
      end
    end

    function setupMake (m: Make)
      var osLower = shell("uname",["-s"])[1].rstrip("\n").translate("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz")

      m.CC = nvcc
      m.CPPFLAGS.push_back("-DTARGET_GPU")
      m.CFLAGS.push_front("-I" + cudaInstallPath + "/include")
      m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib")

      // Clean this up when moving simEngine and simex to subprocess calls for external interfaces (e.g. Matlab)
      if osLower == "darwin" then
        if arch64 then
          error("nVidia tools do not support 64bit architecture.")
        else
          m.TARGET_ARCH = ["-arch", "i386"]
          m.LD = "g++-4.2"
        end
      end

      if arch64 then
	m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib64")
      end

      // nvcc and gcc have different meanings for ARCH so set them specifically for
      // each as part of CFLAGS and LDFLAGS and remove the TARGET_ARCH value
      m.CFLAGS = m.TARGET_ARCH + m.CFLAGS
      m.LDFLAGS = m.TARGET_ARCH + m.LDFLAGS
      m.TARGET_ARCH = []

      // Wrap all gcc flags in --compiler-options when passed to nvcc
      m.CFLAGS = ["--compiler-options", join(" ", m.CFLAGS),
		  "--ptxas-options", join(" ", ptxasFlags),
		  "--opencc-options", join(" ", openccFlags)]
      
      // Currently we use only the first device returned from device_props program
      // which returns a list of available devices sorted by their GFLOPs
      var device_id = Devices.CUDA.getProp(1, "deviceId")
      var device_arch = Devices.CUDA.getProp(1, "arch")
      m.CFLAGS.push_back("-DSIMENGINE_CUDA_DEVICE=" + device_id)
      m.CFLAGS.push_front("-arch=" + device_arch)

      /* Does device debugging really work? I get strange errors from cuda-gdb.
      if debug then
        m.CFLAGS = ["-g", "-G"] + m.CFLAGS
      end
      */

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
				  "gnuplot"] +
				  targetOptions.keys +
				  precisionOptions.keys

  var booleanOptionNamesDebug = ["debug",
				 "emulate",
				 "profile",
				 "nocompile"]

  var numberOptionNamesAlways = ["instances",
				 "start",
				 "stop"]

  var numberOptionNamesDebug = ["parallelmodels",
				"gpuid",
				"gpublocksize"]

  var stringOptionNamesAlways = ["simex",
				 "inputs",
				 "states",
				 "outputs"]
  var stringOptionNamesDebug = []

  var defaultCompilerSettings = {target = "openmp",
				 precision = "double",
				 parallel_models = 1,
				 debug = false, 
				 emulate = false,
				 profile = false}

  var defaultSimulationSettings = {start = 0,
				   instances = 1}

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

    if objectContains(commandLineSettings, "help") then
      printUsage()
    else
      var modelFile = getModelFile(commandLineSettings)
      // If a model was passed as a parameter, compile and/or execute it
      if () <> modelFile then
	var compilerSettings = validateCompilerSettings(commandLineSettings)
	var simulationSettings = validateSimulationSettings(commandLineSettings)
	autoRecompile(modelFile, compilerSettings)
	if objectContains(simulationSettings, "stop") then
	  simulate(FileSystem.realpath(compilerSettings.exfile), simulationSettings)
	else
	  println("Not running: " + compilerSettings.exfile + " " + simulationSettings.contents.tostring())
	end
      end
    end
  end

  function simulate(simulation, simulationSettings)
    var simexCommands = []
    foreach setting in simulationSettings.keys do
      simexCommands.push_back("--" + setting)
      if simulationSettings.getValue(setting) <> true then
	simexCommands.push_back(simulationSettings.getValue(setting).tostring())
      end
    end
    print(join("", shell(simulation, simexCommands)))
  end

  function printUsage()
    println("\nsimEngine usage:\n\n" +
	    "\tSimulation mode: run a simulation from a Diesel model\n" +
	    "\t\tsimEngine [options] -simex <modelfile.dsl>\n\n" +
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
    // FIXME: automatically add the available options from the actual options above
  end

  function getModelFile(commandLineOptions: Table)
    // Set the full realpath of the model file
    if not(objectContains(commandLineOptions, "simex")) then
      ()
    else
      var modelfilepath = FileSystem.realpath(commandLineOptions.getValue("simex"))
      if () == modelfilepath then
	error("The model file '" + commandLineOptions.getValue("simex") + "' does not exist.")
      end
      modelfilepath
    end
  end

  function validateCompilerSettings(commandLineOptions: Table)
    var compilerSettings = {}
    // Check for a single valid target
    var targets = [targetOptions.getValue(target) foreach target in targetOptions.keys when objectContains(commandLineOptions, target)]
    if targets.length() > 1 then
      error("Only one target option can be specified.")
    elseif targets.length() == 1 then
      compilerSettings.add("target", targets.first())
    end

    // Check for a single valid precision
    var precisions = [precisionOptions.getValue(precision) foreach precision in precisionOptions.keys when objectContains(commandLineOptions, precision)]
    if precisions.length() > 1 then
      error("Only one precision option can be specified. (" + join(", ", precisionOptions.keys) + ")")
    elseif precisions.length() == 1 then
      compilerSettings.add("precision", precisionOptions.getValue(precisions.first()))
    end

    if objectContains(commandLineOptions, "debug") then
      compilerSettings.add("debug", true)
    end

    if objectContains(commandLineOptions, "emulate") then
      if("cuda" == compilerSettings.target) then
	compilerSettings.add("emulate", true)
      else
	error("Emulation is only valid for the GPU.")
      end
    end

    if objectContains(commandLineOptions, "profile") then
      compilerSettings.add("profile", true)
    end

    // Add any defaults for settings that were not set from the command-line
    foreach key in defaultCompilerSettings.keys do
      if not(objectContains(compilerSettings, key)) then
	compilerSettings.add(key, defaultCompilerSettings.getValue(key))
      end
    end
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

    // Set all the simulations settings from the commandLineOptions
    if copyOptions(commandLineOptions, simulationSettings, ["start", "stop", "instances", "inputs", "states", "outputs", "gnuplot"]) then
      if not(objectContains(simulationSettings, "stop")) then
	// If any simulation options were set but no stop time was, tell the user this doesn't make sense
	error("In order to simulate a stop time must be specified.")
      else

	// Check if user specified a start time but no stop time
	if objectContains(simulationSettings, "start") and not(objectContains(simulationSettings, "stop")) then
	  error("Specifying a start time has no meaning without a corresponding stop time.")
	end

	// Add any defaults for settings that were not set from the command-line
	foreach key in defaultSimulationSettings.keys do
	  if not(objectContains(simulationSettings, key)) then
	    simulationSettings.add(key, defaultSimulationSettings.getValue(key))
	  end
	end

	// Check if user specified a stop time less than the start time
	if simulationSettings.stop <= simulationSettings.start then
	  error("Stop time must be greater than the start time.")
	end

	// Check for a valid number of instances
	if simulationSettings.instances < 1 or simulationSettings.instances.floor() <> simulationSettings.instances then
	  error("Number of model instances must be an integer value of 1 or more.")
	end
      end
      simulationSettings
    else
      ()
    end
  end

  function autoRecompile (filename: String, compilerSettings: Table)
    if "dsl" <> Path.ext(filename) then
      error ("Unknown type of file " + filename)
    end
    var working_dir = FileSystem.pwd ()
    var dir = Path.dir filename
    var file = Path.file filename
    var simfile = Path.join(working_dir, ((Path.base file) + ".sim"))

    var needsToCompile = true

    var target

    // Instantiating a target could override compilerSettings based on target type (i.e. precision = float for GPU arch_11)
    if "openmp" == compilerSettings.target then
      target = TargetOpenMP.new(compilerSettings)
    elseif "cuda" == compilerSettings.target then
      target = TargetCUDA.new(compilerSettings)
    elseif "cpu" == compilerSettings.target then
      target = TargetCPU.new(compilerSettings)
    else
      error ("Unknown target " + compilerSettings.target)
    end

    // Opening an archive succeeds if the file exists and a manifest can be read.
    var archive = Archive.openArchive (simfile)
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
	      var path = Path.join (dir, i)
	      upToDate = (upToDate and 
			  FileSystem.isfile (path) and
			  creation > FileSystem.modtime (path))
	    end
	    
	    if upToDate then
	      needsToCompile = not (isArchiveCompatibileWithCompilerSettings (archive, compilerSettings))
	    end
	  end
	end
      end
    end

    if needsToCompile then
      compile (filename, target, compilerSettings)
    else
      if compilerSettings.debug then
	println ("reusing existing SIM")
      end
    end
  end

  function isArchiveCompatibileWithCompilerSettings (archive, compilerSettings)
    function compatible (executable)
      var compat = ((executable.target == compilerSettings.target) and
		    (executable.precision == compilerSettings.precision) and
		    (executable.debug or not(compilerSettings.debug)) and
		    (executable.profile == compilerSettings.profile) and
		    (not("cuda" == executable.target)
		     or executable.emulate == compilerSettings.emulate))
      if compat then
	compilerSettings.add("exfile", executable.exfile)
      end
      compat
    end

    () <> Archive.findExecutable (archive, compatible)
  end

  function compile (filename, target, compilerSettings)
    if compilerSettings.debug then
      println ("Compiling " + filename)
    end

    var mod = LF loadModel (filename)
    var name = mod.template.name
    var cname = name + ".c"
    mod.template.settings = compilerSettings

    var instantiatedModel = mod.instantiate()
    var stat = LF compile (instantiatedModel)
    var simfile
    
    if true == stat then
      if "cuda" == compilerSettings.target then
	shell("ln", ["-s", cname, name + ".cu"])
	cname = name + ".cu"
      end

      compilerSettings.add("cSourceFilename", cname)
      
      simfile = Archive.createArchive(name + ".sim", settings.compiler.registry.value, mod.template.imports, target, compilerSettings)
    else
      error(stat)
    end

    stat
  end
//end


