(* simEngine Options (options.sml)
 *
 * This file includes all the default options included in the system
 *)

signature OPTIONS = 
sig

datatype dyntype = FLAG_T | INTEGER_T | REAL_T | STRING_T | INTEGER_VECTOR_T | REAL_VECTOR_T | STRING_VECTOR_T

type group =  {group: string,
	       tag: string,
	       description: string list,
	       visible: bool,
	       options: {short: char option,
			 long: string option,
			 xmltag: string,
			 dyntype: dyntype,
			 description: string list} list}

val getGroupsList : unit -> group list

end

structure OptionsList : OPTIONS=
struct

datatype dyntype = FLAG_T | INTEGER_T | REAL_T | STRING_T | INTEGER_VECTOR_T | REAL_VECTOR_T | STRING_VECTOR_T

type group =  {group: string,
	       tag: string,
	       description: string list,
	       visible: bool,
	       options: {short: char option,
			 long: string option,
			 xmltag: string,
			 dyntype: dyntype,
			 description: string list} list}

val argument_groups =
    [{group="General Options",
      tag="general",
      description=["General System Settings"],
      visible=true,
      options=[{short=SOME #"h",
		long =SOME "help",
		xmltag="help",
		dyntype=FLAG_T,
		description=["Display usage information for simEngine"]},
	       {short=NONE,
		long =SOME "batch",
		xmltag="batch",
		dyntype=STRING_T,
		description=["Run commands directly non interactively from file"]},

	       {short=NONE,
		long=SOME "startupmessage",
		xmltag="startupmessage",
		dyntype=FLAG_T,
		description=["Display the splash message on startup"]},

	       {short=SOME #"v",
		long =SOME "verbose",
		xmltag="verbose",
		dyntype=FLAG_T,
		description=["Verbose printing from compiler"]},

	       {short=SOME #"s",
		long =SOME "simex",
		xmltag="simex",
		dyntype=STRING_T,
		description=["Compile and execute the DSL file as a simulation"]},

	       {short=NONE,
		long =SOME "compile",
		xmltag="compile",
		dyntype=STRING_T,
		description=["Compile the DSL file into a SIM file"]},

	       {short=NONE,
		long =SOME "simulate",
		xmltag="simulate",
		dyntype=STRING_T,
		description=["Simulate the SIM file"]},

	       {short=NONE,
		long =SOME "license-file", 
		xmltag="licenseFile",
		dyntype=STRING_T,
		description=["Specify a user defined license file"]}

     ]},

     {group="Compiler Options",
      tag="compiler",
      description=["Compiler and Code Generation Settings"],
      visible=true,
      options=[
	       (* used in directory overwriting *)
	       {short=SOME #"f",
		long=SOME "force",
		xmltag="force",
		dyntype=FLAG_T,
		description=["Enable/disable force overwriting of directories or files"]},

	       (* keep this - we need to use this one for the SIM file *)
	       {short=SOME #"o",
	       long =SOME "outputname",
	       xmltag="outputname",
	       dyntype=STRING_T,
	       description=["Filename for compilation results"]},

	       (* keep this - we need to use this one for the SIM file *)
	       {short=NONE,
	       long =SOME "outputdir",
	       xmltag="outputdir",
	       dyntype=STRING_T,
	       description=["Directory for simulation results"]},

	       {short=NONE,
		long =NONE,
		xmltag="cSourceFilename",
		dyntype=STRING_T,
		description=["Name of generated source file"]},

	       {short=NONE,
		long =SOME "json_interface",
		xmltag="json_interface",
		dyntype=STRING_T,
		description=["Set output json filename"]},

	       {short=NONE,
		long=SOME "libpath",
		xmltag="librarypath",
		dyntype=STRING_VECTOR_T,
		description=["Location to look for library files"]},

	       {short = NONE,
		long = SOME "registry",
		xmltag = "registry",
		dyntype = STRING_T,
		description = ["Pathname of compiler settings registry file"]},

	       {short=NONE,
		long=SOME "srcpath",
		xmltag="sourcepath",
		dyntype=STRING_VECTOR_T,
		description=["Location to look for source files"]},
	       
	       {short=NONE,
		long=SOME "idepth",
		xmltag="interpreterstackdepth",
		dyntype=INTEGER_T,
		description=["The maximum depth of the interpreter loop stack",
			     "  If the compiler gives infinite loop errors, try adjusting this upwards."]}]},


     {group="Debugging Options",
      tag="debug",
      description=["Options only available in the debug-only version of the system"],
      visible=BuildOptions.devVersion,
      options=[{short=NONE,
		long =SOME "fullform",
		xmltag="usefullform",
		dyntype=FLAG_T,
		description=["Enable/disable use of fullform expression printing"]},
	       {short=NONE,
		long =SOME "genMathematica",
		xmltag="generateMathematica",
		dyntype=FLAG_T,
		description=["Enable/disable generation of textual Mathematica"]}]},
     {group="Internal Processing",
      tag="ir",
      description=["Control the degree of processing within the internal representation"],
      visible=true,
      options=[{short=NONE,
		long=SOME "termRewriteLimit",
		xmltag="termrewritelimit",
		dyntype=INTEGER_T,
		description=["Set limit for number of iterations through evaluating one term rewrite"]}

     ]},


     {group="Logging Options",
      tag="logging",
      description=["Options to control logging of various pieces of information"],
      visible=BuildOptions.devVersion,
      options=[{short=NONE,
		long = SOME "loglibrary",
		xmltag="loglibrary",
		dyntype=FLAG_T,
		description=["Enable/disable logging of library resource information"]},
	       {short=NONE,
		long =SOME "logdof",
		xmltag="logdof",
		dyntype=FLAG_T,
		description=["Enable/disable logging of dof data structure"]},
	       {short=NONE,
		long =SOME "logrewrites",
		xmltag="logrewrites",
		dyntype=FLAG_T,
		description=["Enable/disable logging of expression rewrites"]},
	       {short=NONE,
		long =SOME "logredundancy",
		xmltag="logredundancy",
		dyntype=FLAG_T,
		description=["Enable/disable logging of redundancy elimination"]},
	       {short=NONE,
		long =SOME "logordering",
		xmltag="logordering",
		dyntype=FLAG_T,
		description=["Enable/disable logging of ordering"]},
	       {short=NONE,
		long =SOME "logexternal",
		xmltag="logexternal",
		dyntype=FLAG_T,
		description=["Enable/disable logging of external commands, such as gcc and nvcc"]},
	       {short=NONE,
		long =SOME "logsettings",
		xmltag="logsettings",
		dyntype=FLAG_T,
		description=["Enable/disable logging of settings values"]},
	       {short=NONE,
		long =NONE,
		xmltag="compilerTimingData",
		dyntype=REAL_VECTOR_T,
		description=["Set the timings for the progress bar"]},
	       {short=NONE,
		long =SOME "regenerateTimings",
		xmltag="regenerateTimings",
		dyntype=FLAG_T,
		description=["Regenerate the internal timing data for the progress bar"]}]},

     {group="Optimization Settings",
      tag="optimization",
      description=["Options to control optimizations and information storage bounds"],
      visible=true,
      options=[{short=SOME #"O",
		long=SOME "optimize",
		xmltag="optimize",
		dyntype=FLAG_T,
		description=["Enable/disable optimizations"]},
	       {short=NONE,
		long=SOME "aggregate",
		xmltag="aggregate",
		dyntype=FLAG_T,
		description=["Enable/disable aggregation of iterators"]},
	       {short=NONE,
		long=SOME "redundancy",
		xmltag="redundancy",
		dyntype=FLAG_T,
		description=["Enable/disable elimination of redundancy"]},
	       {short=NONE,
		long=SOME "flatten",
		xmltag="flatten",
		dyntype=FLAG_T,
		description=["Enable/disable internal flattening of model"]}
     ]},

     {group="Installation Settings",
      tag="installation",
      description=["Options related to installation and updating of simEngine"],
      visible=false,
      options=[{short=NONE,
		long =NONE,
		xmltag="updateMajorVersion",
		dyntype=INTEGER_T,
		description=["The major version of latest available release"]},
	       {short=NONE,
		long =NONE,
		xmltag="updateMinorVersion",
		dyntype=INTEGER_T,
		description=["The minor version of latest available release"]},
	       {short=NONE,
		long =NONE,
		xmltag="updateRevision",
		dyntype=STRING_T,
		description=["The revision of latest available release"]},
	       {short=NONE,
		long =NONE,
		xmltag="updateBuildNumber",
		dyntype=INTEGER_T,
		description=["The build number of latest available release"]},
	       {short=NONE,
		long =NONE,
		xmltag="updateBuildDate",
		dyntype=INTEGER_T,
		description=["The build date in number of days since epoch of latest available release"]},
	       {short=NONE,
		long=SOME "depcheck",
		xmltag="depcheck",
		dyntype=FLAG_T,
		description=["Do not load simEngine, only check dependencies and return success/failure in the exit status"]},
	       {short=NONE,
		long =SOME "updateURL",
		xmltag="updateURL",
		dyntype=STRING_T,
		description=["The URL where simEngine updates are located"]}]},

     {group="Simulation Settings",
      tag="simulation",
      description=["Options to control simulation parameters"],
      visible=true,
      options=[{short=SOME #"b",
		long =SOME "start",
		xmltag="start",
		dyntype=REAL_T,
		description=["Starting time for a simulation"]},
	       {short=SOME #"e",
		long =SOME "stop",
		xmltag="stop",
		dyntype=REAL_T,
		description=["Stopping time for a simulation"]},
	       {short=SOME #"n",
		long =SOME "instances",
		xmltag="instances",
		dyntype=INTEGER_T,
		description=["Number of instances to execute"]},
	       {short=NONE,
		long =NONE,
		xmltag="parallel_models",
		dyntype=INTEGER_T,
		description=["Number of instances that the hardware will process in blocks"]},
	       {short=NONE,
		long =SOME "seed",
		xmltag="seed",
		dyntype=INTEGER_T,
		description=["Specific random seed value"]},
	       {short=NONE,
		long =SOME "seeded",
		xmltag="seeded",
		dyntype=FLAG_T,
		description=["Use a specific random seed value specified by the --seed <integer> option"]},
	       {short=NONE,
		long =SOME "binary",
		xmltag="binary",
		dyntype=FLAG_T,
		description=["Enable/disable output generation in binary mode"]},
	       {short=NONE,
		long =SOME "precision",
		xmltag="precision",
		dyntype=STRING_T,
		description=["Set precision to single or double"]},
	       {short=NONE,
		long =SOME "inputs",
		xmltag="inputs",
		dyntype=STRING_VECTOR_T,
		description=["List of inputs set explicitly."]},
	       {short=SOME #"t",
		long =SOME "target",
		xmltag="target",
		dyntype=STRING_T,
		description=["Simulation target platform"]},
	       {short=NONE,
		long =SOME "shared_memory",
		xmltag="shared_memory",
		dyntype=FLAG_T,
		description=["Use shared memory for data transfer"]},
	       {short=NONE,
		long =SOME "buffer_count",
		xmltag="buffer_count",
		dyntype=INTEGER_T,
		description=["Number of buffers in shared memory"]}
     ]},
     {group="GPU Settings",
      tag="gpu",
      description=["Options relating to the graphics processor"],
      visible=true,
      options=[{short=NONE,
		long =SOME "gpuid",
		xmltag="gpuid",
		dyntype=INTEGER_T,
		description=["Set the ID of the GPU that simEngine should utilize"]},
	       {short=NONE,
		long =SOME "gpublocksize",
		xmltag="gpublocksize",
		dyntype=INTEGER_T,
		description=["Override the GPU blocksize - this can be determined automatically by simEngine"]}]},
     {group="Simulation Debug Settings",
      tag="simulation_debug",
      description=["Debugging options to control simulation parameters"],
      visible=BuildOptions.devVersion,
      options=[{short=SOME #"d",
		long =SOME "debug",
		xmltag="debug",
		dyntype=FLAG_T,
		description=["Enable/disable simulation debugging"]},     
	       {short=NONE,
		long =SOME "emulate",
		xmltag="emulate",
		dyntype=FLAG_T,
		description=["Enable/disable GPU emulation mode"]},     
	       {short=NONE,
		long =SOME "gdb",
		xmltag="gdb",
		dyntype=FLAG_T,
		description=["Launch simulation under debugger"]},
	       {short=NONE,
		long =SOME "profile",
		xmltag="profile",
		dyntype=FLAG_T,
		description=["Enable/disable profiling mode"]}]},
     {group="Internal Settings",
      tag="internal",
      description=["Internal compiler settings"],
      visible=false,
      options=[{short=NONE,
		long =SOME "inferior-mode",
		xmltag="inferiorMode",
		dyntype=FLAG_T,
		description=["Enable/disable inferior mode where simEngine works behind another product, ",
			     "for example, simex."]}]}]

fun getGroupsList () =
    argument_groups



end
