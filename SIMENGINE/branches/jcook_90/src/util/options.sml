(* Dynamo Options (options.sml)
 *
 * This file provides functionality for importing options, settings, and 
 * flags from a file and the command line.  See the wiki for more information.
 *)

signature OPTIONS = 
sig

datatype dynvalue = INTEGER of int
		  | REAL of real
		  | STRING of string
		  | INTEGER_VEC of int list
		  | REAL_VEC of real list
		  | STRING_VEC of string list

datatype dyntype = FLAG_T | INTEGER_T | REAL_T | STRING_T | INTEGER_VECTOR_T | REAL_VECTOR_T | STRING_VECTOR_T

datatype dynoption = FLAG of string * bool
		   | SETTING of string * dynvalue

type settings = dynoption list

type group =  {group: string,
	       tag: string,
	       description: string list,
	       visible: bool,
	       options: {short: char option,
			 long: string option,
			 xmltag: string,
			 dyntype: dyntype,
			 description: string list} list}

val optionsdescription : string -> string (* program name -> multi-line string *)

val importRegistryFile : string -> unit

val importCommandLineArgs : string list -> string list

val isFlagSet : string -> bool
val getIntegerSetting : string -> int
val getRealSetting : string -> real
val getStringSetting : string -> string

val getIntegerVectorSetting : string -> int list
val getRealVectorSetting : string -> real list
val getStringVectorSetting : string -> string list

val getGroupsList : unit -> group list

val getSettingsList : unit -> settings
val getSettingsForGroup : string -> settings (*pass the group name *)
val getTagForGroup : string -> string
val getDescriptionForSetting : string -> string list
val getTypeForSetting : string -> dyntype
val setSetting: dynoption -> unit
end

structure DynamoOptions : OPTIONS=
struct

datatype dynvalue = INTEGER of int
		  | REAL of real
		  | STRING of string
		  | INTEGER_VEC of int list
		  | REAL_VEC of real list
		  | STRING_VEC of string list

datatype dyntype = FLAG_T | INTEGER_T | REAL_T | STRING_T | INTEGER_VECTOR_T | REAL_VECTOR_T | STRING_VECTOR_T

datatype dynoption = FLAG of string * bool
		   | SETTING of string * dynvalue

type settings = dynoption list

type group =  {group: string,
	       tag: string,
	       description: string list,
	       visible: bool,
	       options: {short: char option,
			 long: string option,
			 xmltag: string,
			 dyntype: dyntype,
			 description: string list} list}

open Printer

val registryFile = ref "none"
val error = Logger.log_data_error (!registryFile)	
      
val argument_groups =
    [{group="Compiler Options",
      tag="compiler",
      description=["Compiler and Code Generation Settings"],
      visible=BuildOptions.allowSWBackend,
      options=[{short=SOME #"o",
	       long =SOME "filename",
	       xmltag="targetlocation",
	       dyntype=STRING_T,
	       description=["Filename and directory to place the compilation results"]},

	       {short=NONE,
		long=SOME "software",
		xmltag="softwareonly",
		dyntype=FLAG_T,
		description=["Enable/disable creating only a software simulation engine"]},

	       {short=NONE,
		long=SOME "fixedpt",
		xmltag="fixedptonly",
		dyntype=FLAG_T,
		description=["Enable/disable creating only a software and debug simulation engine"]},

	       {short=NONE,
		long=SOME "appendsysname",
		xmltag="appendsysname",
		dyntype=FLAG_T,
		description=["Enable/disable appending of the system name to the directory"]},

	       {short=SOME #"f",
		long=SOME "force",
		xmltag="force",
		dyntype=FLAG_T,
		description=["Enable/disable force overwriting of directories or files"]},

	       {short=SOME #"o",
	       long =SOME "target",
	       xmltag="targetname",
	       dyntype=STRING_T,
	       description=["Filename for compilation results"]},

	       {short=NONE,
		long=SOME "libpath",
		xmltag="librarypath",
		dyntype=STRING_VECTOR_T,
		description=["Location to look for library files"]},

	       {short=NONE,
		long=SOME "srcpath",
		xmltag="sourcepath",
		dyntype=STRING_VECTOR_T,
		description=["Location to look for source files"]},

	       {short=NONE,
		long=NONE,
		xmltag="stdlibrary",
		dyntype=STRING_VECTOR_T,
		description=["The dynamo standard libraries to be used"]},

	       {short=SOME #"l",
		long=SOME "library",
		xmltag="userlibrary",
		dyntype=STRING_VECTOR_T,
		description=["Supply an additional library to link operations from"]},

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
		long =SOME "debugexceptions",
		xmltag="debugexceptions",
		dyntype=FLAG_T,
		description=["Enable/disable exception history printing",
			     "  Note - This only works in the debug version of dynamo"]},
	       {short=NONE,
		long =SOME "fullform",
		xmltag="usefullform",
		dyntype=FLAG_T,
		description=["Enable/disable use of fullform expression printing"]},
	       {short=NONE,
		long =SOME "genC",
		xmltag="generateC",
		dyntype=FLAG_T,
		description=["Enable/disable generation of vanilla C back-end"]},
	       {short=NONE,
		long =SOME "genMathematica",
		xmltag="generateMathematica",
		dyntype=FLAG_T,
		description=["Enable/disable generation of textual Mathematica"]},
	       {short=NONE,
		long =SOME "logdof",
		xmltag="logdof",
		dyntype=FLAG_T,
		description=["Enable/disable logging of dof data structure"]},
	       {short=NONE,
		long =SOME "logrewrites",
		xmltag="logrewrites",
		dyntype=FLAG_T,
		description=["Enable/disable logging of expression rewrites"]}


(*,
	       {short=NONE,
		long=SOME "logdrt",
		xmltag="logdrt",
		dyntype=FLAG_T,
		description=["Enable/disable DRT information logging"]}*)]},
     {group="Internal Processing",
      tag="ir",
      description=["Control the degree of processing within the internal representation"],
      visible=BuildOptions.allowSWBackend,
      options=[{short=NONE,
		long=SOME "TermRewriteLimit",
		xmltag="termrewritelimit",
		dyntype=INTEGER_T,
		description=["Set limit for number of iterations through evaluating one term rewrite"]},
	       {short=SOME #"O",
		long=SOME "Optimiziation",
		xmltag="optimize",
		dyntype=FLAG_T,
		description=["Enable/disable optimizations"]}]},
     

     {group="Logging Options",
      tag="logging",
      description=["Options to control logging of various pieces of information"],
      visible=BuildOptions.devVersion,
      options=[{short=NONE,
		long = SOME "logranges",
		xmltag="logranges",
		dyntype=FLAG_T,
		description=["Enable/disable logging of range to bit information"]},
	       {short=NONE,
		long = SOME "logprecision",
		xmltag="logprecision",
		dyntype=FLAG_T,
		description=["Enable/disable logging of precision propagation information"]},
	       {short=NONE,
		long = SOME "logluts",
		xmltag="logluts",
		dyntype=FLAG_T,
		description=["Enable/disable logging of lookup table generation information"]},
	       {short=NONE,
		long = SOME "logschedule",
		xmltag="logschedule",
		dyntype=FLAG_T,
		description=["Enable/disable logging of hardware scheduling information"]},
	       {short=NONE,
		long = SOME "logdrt",
		xmltag="logdrt",
		dyntype=FLAG_T,
		description=["Enable/disable logging of hardware resource table information"]},
	       {short=NONE,
		long = SOME "logschedtab",
		xmltag="logschedtab",
		dyntype=FLAG_T,
		description=["Enable/disable logging of of the schedule in a tabular format"]},
	       {short=NONE,
		long = SOME "logmemusage",
		xmltag="logmemusage",
		dyntype=FLAG_T,
		description=["Enable/disable logging of hardware memory utilization information"]},
	       {short=NONE,
		long = SOME "lognetlist",
		xmltag="lognetlist",
		dyntype=FLAG_T,
		description=["Enable/disable logging of hardware netlist information"]},
	       {short=NONE,
		long = SOME "logcorrelations",
		xmltag="logcorrelations",
		dyntype=FLAG_T,
		description=["Enable/disable logging of operation correlations"]},
	       {short=NONE,
		long = SOME "logcorrelationscsv",
		xmltag="logcorrelationscsv",
		dyntype=FLAG_T,
		description=["Enable/disable logging of operation correlations in a csv file"]},
	       {short=NONE,
		long = SOME "loglibrary",
		xmltag="loglibrary",
		dyntype=FLAG_T,
		description=["Enable/disable logging of library resource information"]}]},

     {group="Optimization Settings",
      tag="optimization",
      description=["Options to control optimizations and information storage bounds"],
      visible=BuildOptions.allowFPBackend,
      options=[{short=NONE,
		long=SOME "literaltolerance",
		xmltag="literaltolerance",
		dyntype=REAL_T,
		description=["Tolerance of fixed-point representation of literal values"]},	     
	       {short=NONE,
		long= SOME "maxbitwidth",
		xmltag="maxbitwidth",
		dyntype=INTEGER_T,
		description=["Maximum bit width for any operation in the system"]},
	       {short=NONE,
		long= SOME "lutthreshold",
		xmltag="lutthreshold",
		dyntype=INTEGER_T,
		description=["A lower bound on the 'cost' of a region of operations to be turned into a lookup table"]},
	       {short=NONE,
		long= SOME "lutaggregate",
		xmltag="lutaggregate",
		dyntype=REAL_T,
		description=["A weighting used to control selection of tables which match multiple regions of the graph.  1 indicates an even weighting.  <1 and >=0 indicates skewing away from using multiple matches.  >1 exponentially increaes the weighting."]},
	       {short=NONE,
		long= SOME "lutmaxwidth",
		xmltag="lutmaxwidth",
		dyntype=INTEGER_T,
		description=["An upper bound on the number of bits used in each element of a lookup table"]},
	       {short=NONE,
		long= SOME "lutdepth",
		xmltag="lutdepth",
		dyntype=INTEGER_T,
		description=["The size of lookup tables generated"]},
	       {short=NONE,
		long=SOME "inputprecision",
		xmltag="inputprecision",
		dyntype=STRING_T,
		description=["Provide a modified input precision.log file"]},
	       {short=NONE,
		long = SOME "conservativeprecision",
		xmltag="conservativeprecision",
		dyntype=FLAG_T,
		description=["Enable/disable conservative precision estimation"]},
	       {short=NONE,
		long = SOME "normalizeprecision",
		xmltag="normalizeprecision",
		dyntype=FLAG_T,
		description=["Enable/disable normalization of precisions"]}]},

     {group="Scheduler Cost Analysis",
      tag="scheduler",
      description=["Options to adjust weightings for heuristic scheduling"],
      visible=BuildOptions.allowHWBackend,
      options=[{short=NONE,
		long=SOME "preOrderScheduling",
		xmltag="preOrderScheduling",
		dyntype=FLAG_T,
		description=["Enable root-first scheduling (advanced option))"]},
	       {short=NONE,
		long = SOME "weightInpWidth",
		xmltag = "weightInpWidth",
		dyntype = INTEGER_T,
		description = ["Cost function weight for similar input bit width"]},
	       {short=NONE,
		long = SOME "weightInpFrac",
		xmltag = "weightInpFrac",
		dyntype = INTEGER_T,
		description = ["Cost function weight for similar input fractional bit width"]},
	       {short=NONE,
		long = SOME "weightInpSign",
		xmltag = "weightInpSign",
		dyntype = INTEGER_T,
		description = ["Cost function weight for having the same input sign bit"]},
	       {short=NONE,
		long = SOME "weightInpType",
		xmltag = "weightInpType",
		dyntype = INTEGER_T,
		description = ["Cost function weight for input types"]},
	       {short=NONE,
		long = SOME "weightInpUniqueSrcs",
		xmltag = "weightInpUniqueSrcs",
		dyntype = INTEGER_T,
		description = ["Cost function weight for inputs being the same"]},
	       {short=NONE,
		long = SOME "weightInpDly",
		xmltag = "weightInpDly",
		dyntype = INTEGER_T,
		description = ["Cost function weight for delays on inputs"]},
	       {short=NONE,
		long = SOME "weightOpCycleTime",
		xmltag = "weightOpCycleTime",
		dyntype = INTEGER_T,
		description = ["Cost function weight penalizing resources scheduled at later times"]},
	       {short=NONE,
		long = SOME "weightOpScheduled",
		xmltag = "weightOpScheduled",
		dyntype = INTEGER_T,
		description = ["Cost function weight boosting already scheduled resources"]},
	       {short=NONE,
		long = SOME "weightOpLat",
		xmltag = "weightOpLat",
		dyntype = INTEGER_T,
		description = ["Cost function weight for the latency of an operation"]},
	       {short=NONE,
		long = SOME "weightOpType",
		xmltag = "weightOpType",
		dyntype = INTEGER_T,
		description = ["Cost function weight benefiting operations that can be merged"]},
	       {short=NONE,
		long = SOME "weightOpUsedInTree",
		xmltag = "weightOpUsedInTree",
		dyntype = INTEGER_T,
		description = ["Cost function weight reducing tendancy to use one operation twice per equation"]},
	       {short=NONE,
		long = SOME "weightOpLimRsrcs",
		xmltag = "weightOpLimRsrcs",
		dyntype = INTEGER_T,
		description = ["Cost function weight for limiting the number of resources"]},
	       {short=NONE,
		long = SOME "weightOpCorrelation",
		xmltag = "weightOpCorrelation",
		dyntype = INTEGER_T,
		description = ["Cost function weight for helping highly correlated operations be assigned to the same resources"]},
	       {short=NONE,
		long = SOME "weightOutNumSinks",
		xmltag = "weightOutNumSinks",
		dyntype = INTEGER_T,
		description = ["Cost function weight for penalizing fanout"]},
	       {short=NONE,
		long = SOME "weightOutSinkType",
		xmltag = "weightOutSinkType",
		dyntype = INTEGER_T,
		description = ["Cost function weight for helping operations have similar outputs"]}]},

(*     {group="Software Backend Settings",
      options=[{short=NONE,
		long = SOME "swluts",
		xmltag = "swluts",
		dyntype = FLAG_T,
		description = ["Enable/disable software lookup tables"]},
	       (*{short=NONE,
		long = SOME "swfixedpt",
		xmltag = "swfixedpt",
		dyntype = FLAG_T,
		description = ["Enable/disable software based fixed point computation"]},*)
	       {short=NONE,
		long = SOME "fixpt_report",
		xmltag = "fixpt_report",
		dyntype = FLAG_T,
		description = ["Enable/disable software quantization statistics for numerical error analysis"]}
	       ]},*)
      

     {group="Hardware Backend Settings",
      tag="hardware",
      description=["Options to control hardware and device settings"],
      visible=BuildOptions.allowHWBackend,
      options=[{short=NONE,
		long = SOME "iobitwidth",
		xmltag = "iobitwidth",
		dyntype=INTEGER_T,
		description=["The bit width in the I/O FIFO"]},
	       {short=NONE,
		long=SOME "prunenetlist",
		xmltag="prunenetlist",
		dyntype=FLAG_T,
		description=["Enable/disable pruning of unused verilog components/modules"]},
	       {short=NONE,
		long = SOME "hwwrap",
		xmltag="hwwrap",
		dyntype=FLAG_T,
		description=["Enable/disable wrapping operations into previous/subsequent iterations (improves performance)"]},
	       {short=NONE,
		long = SOME "outputrate",
		xmltag = "outputrate",
		dyntype=INTEGER_T,
		description=["The downsampling rate for outputs (number of simulation iterations per sample)"]},
	       {short=NONE,
		long = SOME "clkfreq",
		xmltag = "clkfreq",
		dyntype=REAL_T,
		description=["The desired clock frequency of the model execution core (in MHz)"]},
	       {short=NONE,
		long =NONE,
		xmltag = "sysclkdiv",
		dyntype=INTEGER_T,
		description=["System clock division factor (2=50 MHz, 4=25 MHz)"]},
	       {short=NONE,
		long = SOME "gapscale",
		xmltag = "gapscale",
		dyntype=REAL_T,
		description=["The scaling factor for General Area Primitives (GAPs) during resource constraining"]},
	       {short=NONE,
		long = SOME "multscale",
		xmltag = "multscale",
		dyntype=REAL_T,
		description=["The scaling factor for embedded multipliers during resource constraining"]},
	       {short=NONE,
		long = SOME "ramscale",
		xmltag = "ramscale",
		dyntype=REAL_T,
		description=["The scaling factor for block RAMs during resource constraining"]},
	       {short=NONE,
		long=SOME "skip_area_check",
		xmltag = "skip_area_check",
		dyntype=FLAG_T,
		description=["Skips area verification and allows netlisting to continue even if the design won't fit in the hardware"]},
	       {short=NONE,
		long = SOME "schedulerpasslimit",
		xmltag = "schedulerpasslimit",
		dyntype=INTEGER_T,
		description=["The maximum number of iterations for the hardware resource scheduler"]},
	       {short=SOME #"t",
		long =SOME "hwtarget",
		xmltag="hwtarget",
		dyntype=STRING_T,
		description=["Hardware platform to target",
			     "  Choices are {ml402, XtremeDSP_V2, XtremeDSP_V4}"]},
	       {short=NONE,
		long=NONE,
		xmltag="deviceFamily",
		dyntype=STRING_T,
		description=["FPGA device family, choices are {virtexII, virtex4, virtex5}"]},
	       {short=NONE,
		long=NONE,
		xmltag="devicePart",
		dyntype=STRING_T,
		description=["FPGA device part number"]},
	       {short=NONE,
		long=NONE,
		xmltag="deviceSpeed",
		dyntype=STRING_T,
		description=["FPGA device speed rating"]},
	       {short=NONE,
		long=NONE,
		xmltag="devicePackage",
		dyntype=STRING_T,
		description=["FPGA device package"]},
	       {short=NONE,
		long=NONE,
		xmltag="jtagPort",
		dyntype=STRING_T,
		description=["JTAG programmer port"]},
	       {short=NONE,
		long=SOME "IPaddress",
		xmltag="IPAddress",
		dyntype=STRING_T,
		description=["Dynamo modeling platform IP address"]}
	      ]
     }
		
    ]

val valid_options = GeneralUtil.flatten (map (fn({options, ...}) => options) argument_groups)

val emptysettings = []

val desc_column_start = 30

fun typeOfSetting n =
    case List.find (fn({xmltag, ...}) => n = xmltag) valid_options of
	SOME {dyntype, ...} => dyntype
      | NONE => DynException.stdException ("Invalid or unknown setting '" ^ n ^ "'", 
					   "DynamoOptions.typeOfSetting", 
					   Logger.OTHER)

fun typeOfDynValue (INTEGER _) = INTEGER_T
  | typeOfDynValue (REAL _) = REAL_T
  | typeOfDynValue (STRING _) = STRING_T
  | typeOfDynValue (INTEGER_VEC _) = INTEGER_VECTOR_T
  | typeOfDynValue (REAL_VEC _) = REAL_VECTOR_T
  | typeOfDynValue (STRING_VEC _) = STRING_VECTOR_T

fun type2str typ =
    case typ of
	FLAG_T => "flag"
      | INTEGER_T => "integer"
      | REAL_T => "real"
      | STRING_T => "string"
      | INTEGER_VECTOR_T => "integer vector"
      | REAL_VECTOR_T => "real vector"
      | STRING_VECTOR_T => "string vector"

fun option2description {short=NONE, long=NONE, xmltag, dyntype, description} =
    nil
  | option2description {short, long, xmltag, dyntype, description} =
    let
	val prefix = case dyntype of
			 FLAG_T => "[+/-]"
		       | _ => "-"

	val argcolumn = "  "
	val argcolumn = case short of
			    SOME short => argcolumn ^ prefix ^ (Char.toString short)
			  | _ => argcolumn
	val argcolumn = case (short, long) of
			    (SOME _, SOME _) => argcolumn ^ ", "
			  | _ => argcolumn
	val argcolumn = case long of
			    SOME long => argcolumn ^ prefix ^ long
			  | _ => argcolumn
	val argcolumn = case dyntype of 
			    FLAG_T => argcolumn
			  | INTEGER_T => argcolumn ^ " <int>"
			  | REAL_T => argcolumn ^ " <real>"
			  | STRING_T => argcolumn ^ " <string>"
			  | INTEGER_VECTOR_T => argcolumn ^ " [<int> ...]"
			  | REAL_VECTOR_T => argcolumn ^ " [<real> ...]"
			  | STRING_VECTOR_T => argcolumn ^ " [<string> ...]"

	(*
	 val _ = 
	     if String.size (argcolumn) > desc_column_start then
		 Logger.log Logger.INTERNAL Logger.ERROR ($("argument column size too small for argument " ^ xmltag))
	     else
		 ()
	 *)

	fun whitespace length =
	    if length <= 0 then
		""
	    else
		" " ^ (whitespace (length - 1))

	val (first_description, rest_description) = case description of
							nil => ("", nil)
						      | (first :: rest) => (first, rest)
    in
	(argcolumn ^ (whitespace (desc_column_start - (String.size argcolumn))) ^ " " ^ first_description)
	:: (map (fn(d) => (whitespace (desc_column_start + 1)) ^ d)  rest_description)
    end

fun group2description {group, visible, options, ...} =
    if visible then
	group :: (GeneralUtil.flatten (map option2description options))
    else
	[]

fun optionsdescription (progname) =
    let
	val header = [Globals.name ^ " (" ^ Globals.version ^ ")",
		      "Usage: " ^ progname ^ " [options] [<source file> ...]",
		      "Options:"]
	val footer = ["",
		      "For more information consult the documentation or visit " ^ Globals.simatra_url]
	val lines = (*header @*) (GeneralUtil.flatten (map group2description argument_groups)) (*@ footer*)
    in
	String.concatWith "\n" lines
    end
    handle e => raise e before print "Internal error: DynamoOptions.optionsdescription\n"

fun strtail "" = ""
  | strtail str =
    implode (tl (explode str))

fun strhead "" = DynException.stdException ("received empty string", "DynamoOptions.strhead", Logger.INTERNAL)
  | strhead str =
    hd (explode str)

fun removeEntry (name, nil) = 
    nil
  | removeEntry (name, FLAG (n, v) :: rest) =
    if n = name then
	rest
    else
	(FLAG(n, v)) :: (removeEntry (name, rest))
  | removeEntry (name, SETTING (n, v) :: rest) =
    if n = name then
	rest
    else
	(SETTING(n, v)) :: (removeEntry (name, rest))

fun enableChar2bool enable =
    case enable of
	#"+" => true
      | #"-" => false
      | _ => DynException.stdException ("Unexpected flag value", "DynamoOptions.enableChar2bool", Logger.INTERNAL)
	     
fun addFlag(name, value, settings) =
    let
	val settings' = removeEntry (name, settings)
    in
	FLAG(name, value) :: settings'
    end

fun addSetting(name, dyntype, argument, settings) =
    let
	exception InvalidArgument 
    in
	let
	    
	    val settings' = removeEntry (name, settings)
	    val value = case dyntype of
			    INTEGER_T => 
			    (case Int.fromString argument of
				 SOME i =>
				 INTEGER i
			      | NONE =>
				(error ($("Argument for '" ^ name ^ "' not a valid integer"));
				 raise InvalidArgument))
			  | REAL_T => 
			    (case Real.fromString argument of
				 SOME r =>
				 REAL r
			       | NONE =>
				 (error ($("Argument for '" ^ name ^ "' not a valid real"));
				  raise InvalidArgument))
			  | STRING_T => 
			    STRING (argument)
			  | INTEGER_VECTOR_T =>
			    (case Int.fromString argument of
				 SOME i =>
				 INTEGER_VEC [i]
			       | NONE => (error ($("Argument for '" ^ name ^ "' not a valid integer"));
					  raise InvalidArgument))
			  | REAL_VECTOR_T => 
			    (case Real.fromString argument of
				 SOME r =>
				 REAL_VEC [r]
			       | NONE =>
				 (error ($("Argument for '" ^ name ^ "' not a valid real"));
				  raise InvalidArgument))
			  | STRING_VECTOR_T => (* deliminate arguments by a : *)			    
			    STRING_VEC (*[argument]*) (String.tokens (fn(c)=> c = #":") argument)
			  | FLAG_T =>
			    DynException.stdException ("A flag dyntype is unexpected for a setting", "DynamoOptions.addSetting", Logger.INTERNAL)
			    
	    fun is_vector (STRING_VEC _) = true
	      | is_vector (INTEGER_VEC _) = true
	      | is_vector (REAL_VEC _) = true
	      | is_vector _ = false

	    fun combine_vectors (STRING_VEC v1, STRING_VEC v2) = STRING_VEC (v1 @ v2)
	      | combine_vectors (INTEGER_VEC v1, INTEGER_VEC v2) = INTEGER_VEC (v1 @ v2)
	      | combine_vectors (REAL_VEC v1, REAL_VEC v2) = REAL_VEC (v1 @ v2)
	      | combine_vectors _ =
		(error ($("Argument for vector '" ^ name ^ "' not same type as pre-existing vector"));
		 raise InvalidArgument)

	    fun replace (n,v) nil =
		[SETTING(n,v)]
	      | replace (n,v) ((SETTING(n', v'))::rest) =
		if n = n' then
		    (if is_vector v then
			 (SETTING (n, combine_vectors(v, v'))) :: rest
		     else
			 (SETTING (n,v)) :: rest)
		else
		    (SETTING (n', v')) :: (replace (n,v) rest)
	      | replace (n,v) (flag::rest) =
		flag :: (replace (n,v) rest)
		

	    val settings'' = 
		replace (name, value) settings'
	in
	    settings'' 
	end	
	    handle InvalidArgument 
		   => settings
		      before DynException.setErrored()

    end
				  
fun isCLOption name {short, long, ...} =
    (case short of
	 SOME c => name = (Char.toString c)
       | _ => false)
    orelse
    (case long of
	 SOME s => name = ("-" ^ s) orelse name = s
       | _ => false)

fun entry2name {xmltag, ...} = xmltag
fun entry2type {dyntype, ...} = dyntype

fun processCommandLineArgs (settings, files, nil) = (settings, files)
  | processCommandLineArgs (settings, files, arg::rest) = 
    let
	exception ArgFailure

	fun isPlusOrMinus c = c = #"+" orelse c = #"-"

	fun entryIsFlag {dyntype=FLAG_T, ...} = true
	  | entryIsFlag _ = false

    in
	if isPlusOrMinus (strhead arg) then
	    (case List.find (isCLOption (strtail arg)) valid_options of
		 SOME (entry as {xmltag, ...}) => 
		 (Logger.log_notice ($("Adding setting '" ^ xmltag ^ "' for command line argument '" ^ (strtail arg) ^ "'"));
		  if entryIsFlag(entry) then 
		      processCommandLineArgs(addFlag (entry2name entry, enableChar2bool(strhead arg), settings),
					     files,
					     rest)
		  else (* entry is a setting *)
		      let
			  val (argument, rest) = case rest of
						     nil => (error ($("Argument expected for '" ^ (strtail arg) ^ "'"));
							     raise ArgFailure)
						   | argument::rest =>
						     if isPlusOrMinus (strhead argument) then
							 (error ($("Argument expected for '" ^ (strtail arg) ^ "'"));
							  raise ArgFailure)
						     else
							 (argument, rest)
		      in
			  processCommandLineArgs(addSetting(entry2name entry, entry2type entry, argument, settings), 
						 files,
						 rest)
		      end)
	       | NONE => (Logger.log_error ($("Invalid argument '" ^ (strtail arg) ^ "'"));
			  DynException.setErrored();
			  processCommandLineArgs(settings, files, rest)))
		       
	else
	    processCommandLineArgs(settings, arg::files, rest)
		       
    end

val settings = ref emptysettings

fun importCommandLineArgs (args) =
    let
(*	val _ = StatusReporter.beginProcess("Parsing command line arguments", 1)*)

	val (s, input_files) = processCommandLineArgs (!settings, nil, args)
(*	val _ = StatusReporter.reportWork(1)*)

	val _ = settings := s
    in
	input_files
    end 
	handle e => (DynException.log "DynamoOptions.importCommandLineArgs" e;
		     DynException.setErrored();
		     [])

fun importRegistryEntry (Registry.REG_FLAG(n, f), existingSettings) =
    (addFlag(n, f, existingSettings)
     handle e => (DynException.log "DynamoOptions.importRegistryEntry(FLAG, ...)" e;
		  existingSettings))
  | importRegistryEntry (Registry.REG_SETTING(n, f), existingSettings) =
    let
	val (dyntype,arg) = case f of
				Registry.REG_NUMBER r 
				=> (case typeOfSetting n of
					REAL_T => (REAL_T, Real.toString r)
				      | INTEGER_T => (INTEGER_T, Int.toString (Real.floor r))
				      | REAL_VECTOR_T => (REAL_VECTOR_T, Real.toString r)
				      | INTEGER_VECTOR_T => (INTEGER_VECTOR_T, Int.toString (Real.floor r))
				      | _ => DynException.stdException ("Setting '" ^ n ^ "' had non-numerical argument", 
									"DynamoOptions.importRegistryEntry", 
									Logger.DATA))
			      | Registry.REG_STRING s 
				=> (case typeOfSetting n of
					STRING_T => (STRING_T, s)
				      | STRING_VECTOR_T => (STRING_VECTOR_T, s)
				      | _ => DynException.stdException ("Setting '" ^ n ^ "' had non-string argument", 
									"DynamoOptions.importRegistryEntry", 
									Logger.DATA))
						
    in
	addSetting (n, dyntype, arg, existingSettings)
    end
	handle e => (DynException.log "DynamoOptions.importRegistryEntry(SETTING, ...)" e;
		     existingSettings)


fun importRegistryFile (file) = 
    (if GeneralUtil.isFile file then
	 (registryFile := file;
	  settings := foldl importRegistryEntry (!settings) (DynRegParse.parse(file));
	  ())
     else
	 (Logger.log_failure ($("Can't read registry file '"^file^"'"));
	  DynException.setErrored();
	  DynException.checkToProceed()))
    handle e => DynException.checkpoint "DynamoOptions.importRegistryFile" e


fun getSettingName (FLAG (n, _)) = n
  | getSettingName (SETTING (n, _)) = n

fun isNamedFlag name s =
    case s of
	FLAG (n, _) => n = name
      | _ => false

fun isNamedSetting name s =
    case s of
	SETTING(n, _) => n = name
      | _ => false

(* scalar getters *)	     
fun isFlagSet(name) =
    case List.find (isNamedFlag name) (!settings) of
	SOME (FLAG(_, flag)) 
	=> flag
      | SOME _ 
	=> DynException.stdException ("'" ^ name ^ "' is not a flag as expected", 
				      "DynamoOptions.isFlagSet", 
				      Logger.DATA)
      | NONE 
	=> 
	DynException.stdException ("Flag '" ^ name ^ "' does not exist", 
				   "DynamoOptions.isFlagSet", 
				   Logger.DATA)
	
fun getIntegerSetting(name) =
    case List.find (isNamedSetting name) (!settings) of
	SOME (SETTING(_, INTEGER s)) 
	=> s
      | SOME _ 
	=> DynException.stdException ("Setting '" ^ name ^ "' was not an integer as expected", 
				      "DynamoOptions.getIntegerSetting", 
				      Logger.DATA)
      | NONE 
	=> DynException.stdException ("Setting '" ^ name ^ "' does not exist", 
				      "DynamoOptions.getIntegerSetting", 
				      Logger.DATA)

fun getRealSetting(name) =
    case List.find (isNamedSetting name) (!settings) of
	SOME (SETTING(_, REAL r)) 
	=> r
      | SOME _ 
	=> DynException.stdException ("Setting '" ^ name ^ "' was not a real as expected", 
				      "DynamoOptions.getRealSetting", 
				      Logger.DATA)
      | NONE 
	=> DynException.stdException ("Setting '" ^ name ^ "' does not exist", 
				      "DynamoOptions.getRealSetting", 
				      Logger.DATA)

fun getStringSetting(name) =
    case List.find (isNamedSetting name) (!settings) of
	SOME (SETTING(_, STRING s)) 
	=> s
      | SOME _ 
	=> DynException.stdException ("Setting '" ^ name ^ "' was not a string as expected", 
				      "DynamoOptions.getStringSetting", 
				      Logger.DATA)
      | NONE 
	=> DynException.stdException ("Setting '" ^ name ^ "' does not exist", 
				      "DynamoOptions.getStringSetting", 
				      Logger.DATA)


(* vector getters *)
fun getIntegerVectorSetting(name) =
    case List.find (isNamedSetting name) (!settings) of
	SOME (SETTING(_, INTEGER_VEC s)) 
	=> s
      | SOME _ 
	=> DynException.stdException ("Setting '" ^ name ^ "' was not an integer vector as expected", 
				      "DynamoOptions.getIntegerSetting", 
				      Logger.DATA)
      | NONE 
	=> []

fun getRealVectorSetting(name) =
    case List.find (isNamedSetting name) (!settings) of
	SOME (SETTING(_, REAL_VEC r)) 
	=> r
      | SOME _ 
	=> DynException.stdException ("Setting '" ^ name ^ "' was not a real vector as expected", 
				      "DynamoOptions.getRealSetting", 
				      Logger.DATA)
      | NONE 
	=> []

fun getStringVectorSetting(name) =
    case List.find (isNamedSetting name) (!settings) of
	SOME (SETTING(_, STRING_VEC s)) 
	=> s
      | SOME _ 
	=> DynException.stdException ("Setting '" ^ name ^ "' was not a string vector as expected", 
				      "DynamoOptions.getStringSetting", 
				      Logger.DATA)
      | NONE 
	=> []


fun getSettingsList () =
    !settings

fun getDescriptionForSetting settingname =
    case (List.find (fn({xmltag, ...}) => settingname = xmltag) valid_options) of
	SOME {description, ...} => description
      | NONE => DynException.stdException ("Setting " ^ settingname ^ " was not found", "DynamoOptions.getDescriptionForSetting", Logger.INTERNAL)

fun getTypeForSetting settingname =
    case (List.find (fn({xmltag, ...}) => settingname = xmltag) valid_options) of
	SOME {dyntype, ...} => dyntype
      | NONE => DynException.stdException ("Setting " ^ settingname ^ " was not found", "DynamoOptions.getTypeForSetting", Logger.INTERNAL)



fun getGroupsList () =
    argument_groups

fun isInGroup groupname setting =
    let
	val valid_options = case List.find (fn({group,...})=> group = groupname) argument_groups of
				SOME {options, ...} => options
			      | NONE => []
    in
	List.exists (fn({xmltag, ...}) => xmltag = (getSettingName setting)) valid_options
    end
	

fun getSettingsForGroup groupname =
    List.filter (isInGroup groupname) (!settings)

fun getTagForGroup groupname = 
    case List.find (fn({group,...})=> group = groupname) argument_groups of
	SOME {tag, ...} => tag
      | NONE => DynException.stdException ("Group " ^ groupname ^ " was not found", "DynamoOptions.getTagForGroup", Logger.INTERNAL)

fun setSetting (SETTING(name, value)) =
    let
	(* make sure the value is of the correct type *)
	val _ = 
	    if (typeOfSetting name) = (typeOfDynValue value) then
		()
	    else
		DynException.stdException ("Wrong type value received for " ^ name ^ ", expected " ^ (type2str (typeOfSetting name)) ^ " and received " ^ (type2str (typeOfDynValue value)), "DynamoOptions.setSetting", Logger.INTERNAL)
    in
	settings := (SETTING(name, value)) :: (List.filter (fn(n) => (not (isNamedSetting name n))) (!settings))
    end
    
  | setSetting (FLAG(name, value)) =
    settings := addFlag(name, value, !settings)
end
