(* Dynamo Options (options_process.sml)
 *
 * This file provides functionality for importing options, settings, and 
 * flags from a file and the command line.  See the wiki for more information.
 *)

signature OPTIONSPROCESS = 
sig

datatype dynvalue = INTEGER of int
		  | REAL of real
		  | STRING of string
		  | INTEGER_VEC of int list
		  | REAL_VEC of real list
		  | STRING_VEC of string list

datatype dynoption = FLAG of string * bool
		   | SETTING of string * dynvalue

type settings = dynoption list

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

val getSettingsList : unit -> settings
val getSettingsForGroup : string -> settings (*pass the group name *)
val getTagForGroup : string -> string
val getDescriptionForSetting : string -> string list
val getTypeForSetting : string -> OptionsList.dyntype
val setSetting: dynoption -> unit

val logSettings: unit -> unit

(* JSON options *)
val allSettingsToJSON: unit -> JSON.json
val settingsToJSON: string list -> JSON.json


end

structure DynamoOptions : OPTIONSPROCESS =
struct

open OptionsList;

datatype dynvalue = INTEGER of int
		  | REAL of real
		  | STRING of string
		  | INTEGER_VEC of int list
		  | REAL_VEC of real list
		  | STRING_VEC of string list

datatype dynoption = FLAG of string * bool
		   | SETTING of string * dynvalue

type settings = dynoption list

open Printer

val registryFile = ref "none"
fun error msg = (Logger.log_data_error (!registryFile) msg;
		 DynException.setErrored())
      

val argument_groups = OptionsList.getGroupsList()
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
			 (*FLAG_T => "[+/-]"
		       | *)_ => "-"

	val longprefix = "--"

	val suffix = case dyntype of
			 FLAG_T => "[=false]"
		       | _ => ""

	val argcolumn = "  "
	val argcolumn = case short of
			    SOME short => argcolumn ^ prefix ^ (Char.toString short)
			  | _ => argcolumn
	val argcolumn = case (short, long) of
			    (SOME _, SOME _) => argcolumn ^ ", "
			  | _ => argcolumn
	val argcolumn = case long of
			    SOME long => argcolumn ^ longprefix ^ long ^ suffix
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
	(group :: (GeneralUtil.flatten (map option2description options))) @ [""]
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
    handle e => DynException.checkpoint "DynamoOptions.optionsdescription" e

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


fun testSetting arg =
    let
	fun isEquals #"=" = true
	  | isEquals _ = false
	val tkns = String.tokens isEquals arg
	val valid = if List.length tkns < 1 orelse List.length tkns > 2 then
			(error ($("Setting '" ^ arg ^ "' had unexpected argument"));
			 false)
		    else
			true
    in
	if valid andalso List.length tkns = 2 then (* there's an equals sign *)
	    if GeneralUtil.strcmpi (List.nth (tkns,1), "true") then
		true
	    else if GeneralUtil.strcmpi (List.nth (tkns,1), "false") then
		false
	    else 
		(error ($("Setting '" ^ (List.nth (tkns,0)) ^ "' had unexpected argument '"^(List.nth (tkns,1))^"', expecting 'true' or 'false'"));
		 false (* just say false to go on *))
		
	else (* just by itself *)
	    true
    end
	     
fun addFlag(name, value, settings) =
    let
	(*val _ = Logger.log_notice ($("Adding setting: " ^ name ^ " to " ^ (GeneralUtil.bool2str value)))*)
	val settings' = removeEntry (name, settings)
    in
	FLAG(name, value) :: settings'
    end

fun addSetting(name, dyntype, argument, settings) =
    let
	val split_str = String.tokens (fn(c)=>c= #":")
	val settings' = removeEntry (name, settings)
	val value = case dyntype of
			INTEGER_T => 
			(case Int.fromString argument of
			     SOME i =>
			     INTEGER i
			   | NONE =>
			     (error ($("Argument '"^ argument ^"' for '" ^ name ^ "' not a valid integer"));
			      INTEGER 0))
		      | REAL_T => 
			(case Real.fromString argument of
			     SOME r =>
			     REAL r
			   | NONE =>
			     (error ($("Argument '"^ argument ^"' for '" ^ name ^ "' not a valid number"));
			      REAL 0.0))
		      | STRING_T => 
			STRING (argument)
		      | INTEGER_VECTOR_T =>
			let
			    val i_vec_opt = map Int.fromString (split_str argument)
			    val _ = app 
					(fn(i) => case i of
						      SOME _ => ()
						    | NONE => error ($("Argument '"^ argument ^"' for '" ^ name ^
								       "' does not have all valid integers"))) 
					i_vec_opt
			    val i_vec = List.mapPartial (fn(i)=>i) i_vec_opt
			in
			    INTEGER_VEC i_vec
			end
		      | REAL_VECTOR_T => 
			let
			    val r_vec_opt = map Real.fromString (split_str argument)
			    val _ = app 
					(fn(r) => case r of
							    SOME _ => () 
							  | NONE => error ($("Argument '"^ argument ^"' for '" ^ name ^
									     "' does not have all valid numbers")))
					r_vec_opt
			    val r_vec = List.mapPartial (fn(r)=>r) r_vec_opt
			in
			    REAL_VEC r_vec
			end
		      | STRING_VECTOR_T => (* deliminate arguments by a : *)			    
			STRING_VEC (*[argument]*) (split_str argument)
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
	    (error ($("Argument for vector '" ^ name ^ "' is not same type as pre-existing vector"));
	     STRING_VEC [])

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
    (*handle DynException.SettingsError 
	   => settings
	      before DynException.setErrored()*)

				  
fun isCLOption name {short, long, ...} =
    let
	fun isEquals (#"=") = true
	  | isEquals _ = false
	fun upToEquals str =
	    let val tkns = String.tokens isEquals str
	    in if List.length tkns > 0 then List.hd tkns else ""
	    end
	val name' = upToEquals name
    in
	(case short of
	     SOME c => name' = (Char.toString c)
	   | _ => false)
	orelse
	(case long of
	     SOME s => name' = ("-" ^ s) orelse name' = s
	   | _ => false)
    end
fun entry2name {xmltag, ...} = xmltag
fun entry2type {dyntype, ...} = dyntype

fun processCommandLineArgs (settings, files, nil) = (settings, files)
  | processCommandLineArgs (settings, files, arg::rest) = 
    let
	fun isPlusOrMinus c = c = #"+" orelse c = #"-"

	fun entryIsFlag {dyntype=FLAG_T, ...} = true
	  | entryIsFlag _ = false

    in
	if isPlusOrMinus (strhead arg) then
	    (case List.find (isCLOption (strtail arg)) valid_options of
		 SOME (entry as {xmltag, ...}) => 
		 ((*Logger.log_notice ($("Adding setting '" ^ xmltag ^ "' for command line argument '" ^ (strtail arg) ^ "'"));*)
		  if entryIsFlag(entry) then 
		      let
			  val res = testSetting arg
			  val _ = Logger.log_notice ($("Setting '" ^ xmltag ^ "' command line argument flag to '" ^ (GeneralUtil.bool2str res) ^ "'"))
		      in
			  processCommandLineArgs(addFlag (entry2name entry, res, settings),
						 files,
						 rest)
		      end
		  else (* entry is a setting *)
		      let
			  val (argument, rest) = case rest of
						     nil => (error ($("Argument expected for '" ^ (strtail arg) ^ "'"));
							     ("undefined", []))
						   | argument::rest =>
						     if not("-" = argument) andalso isPlusOrMinus (strhead argument) then
							 (error ($("Argument expected for '" ^ (strtail arg) ^ "'"));
							  ("undefined", rest))
						     else
							 (argument, rest)
			  val _ = Logger.log_notice ($("Setting '" ^ xmltag ^ "' command line argument setting to '" ^ argument ^ "'"))
		      in
			  processCommandLineArgs(addSetting(entry2name entry, entry2type entry, argument, settings), 
						 files,
						 rest)
		      end)
	       | NONE => (Logger.log_error ($("Unrecognized argument '" ^ (strtail arg) ^ "' passed on the command line"));
			  DynException.setErrored();
			  processCommandLineArgs(settings, files, rest)))
		       
	else
	    processCommandLineArgs(settings, arg::files, rest)
		       
    end

val settings = ref emptysettings

fun importCommandLineArgs (args) =
    let
(*	val _ = StatusReporter.beginProcess("Parsing command line arguments", 1)*)

	val _ = registryFile := "Command Line"
	val (s, input_files) = processCommandLineArgs (!settings, nil, args)
(*	val _ = StatusReporter.reportWork(1)*)

	val _ = settings := s
    in
	input_files
    end 
    handle e => DynException.checkpoint "DynamoOptions.importCommandLineArgs" e

fun importRegistryEntry file (Registry.REG_FLAG(n, f), existingSettings) =
    (addFlag(n, f, existingSettings)
     handle e => (DynException.log "DynamoOptions.importRegistryEntry(FLAG, ...)" e;
		  existingSettings))
  | importRegistryEntry file (Registry.REG_SETTING(n, f), existingSettings) =
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
			      | Registry.REG_NUMBER_VECTOR rlist 
				=> (case typeOfSetting n of
				   	REAL_T => (Logger.log_data_error file (Printer.$("Setting '"^n^"' had an unexpected vectorized argument when a scalar value was expected."));
						  DynException.setErrored();
						  (REAL_T, "0.0"))
				      | INTEGER_T => (Logger.log_data_error file (Printer.$("Setting '"^n^"' had an unexpected vectorized argument when a scalar value was expected."));
						      DynException.setErrored();
						      (INTEGER_T, "0"))
				      | REAL_VECTOR_T => (REAL_VECTOR_T, String.concatWith ":" (map Real.toString rlist))
				      | INTEGER_VECTOR_T => (INTEGER_VECTOR_T, String.concatWith ":" (map (Int.toString o Real.floor) rlist))
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
	  settings := foldl (importRegistryEntry file)
			    (! settings) 
			    ((Registry.REG_SETTING ("registry", Registry.REG_STRING file)) :: (DynRegParse.parse(file)));
	  ())
     else
	 (error ($("Can't read registry file '"^file^"'"));
	  DynException.setErrored()))
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

(* logSettings: display the settings on the screen in a table-like format.  This can be invoked with the -logoptions command line flag. *)
fun logSettings () =
    let
	fun settingToID (FLAG (id, _)) = id
	  | settingToID (SETTING (id, _)) = id

	val max_size = foldl (fn(a,b)=> if a > b then a else b) 0 (map (String.size o settingToID) (!settings))
	fun padstr len str = 
	    let
		val size = String.size str
	    in
		if len >= size then
		    str ^ (String.concat (List.tabulate (len-size, fn(x)=>" ")))
		else
		    DynException.stdException("Unexpected string input", "OptionsProcess.logSettings.padstr", Logger.INTERNAL)
	    end
	val padstr = padstr (max_size+2)
	    
	fun disp_setting (FLAG (id, bool))= padstr (id ^ ": ") ^ (GeneralUtil.bool2str bool)
	  | disp_setting (SETTING (id, optval))= padstr (id ^ ": ") ^
						 (case optval of
						      INTEGER i => GeneralUtil.int2str i
						    | REAL r => GeneralUtil.real2str r
						    | STRING s => "\"" ^ s ^ "\""
						    | INTEGER_VEC ivec => GeneralUtil.list2str GeneralUtil.int2str ivec
						    | REAL_VEC rvec => GeneralUtil.list2str GeneralUtil.real2str rvec
						    | STRING_VEC svec => GeneralUtil.strlist2str svec)
	    
    in
	(print("\nSystem Options\n");
	 print("--------------------------------\n");
	 (app (fn(s)=> print(disp_setting s ^ "\n")) (!settings));
	 print("\n\n"))
    end

local
    open JSON
    open JSONExtensions
    val int = int o IntInf.fromInt

in

fun settingsListToJSON settings =
    array (map settingToJSON settings)
and settingToJSON (FLAG (id, bool')) = 
    JSONTypedObject ("FLAG", object [("id", string id),
				     ("bool", bool bool')])
  | settingToJSON (SETTING (id, optval)) = 
    JSONTypedObject ("SETTING", object [("id", string id),
					("optval", optvalToJSON optval)])
and optvalToJSON (INTEGER i) = int i
  | optvalToJSON (REAL r) = real r
  | optvalToJSON (STRING s) = string s
  | optvalToJSON (INTEGER_VEC ivec) = array (map int ivec)
  | optvalToJSON (REAL_VEC rvec) = array (map real rvec)
  | optvalToJSON (STRING_VEC svec) = array (map string svec)

fun allSettingsToJSON () = settingsListToJSON (!settings)

end


fun settingsToJSON settings_list = 
    let
	val (exists, notexists) = List.partition (fn(id)=> List.exists (fn(s)=> getSettingName s = id) (!settings)) settings_list
	val _ = if List.length notexists > 0 then
		    Logger.log_warning(Printer.$("Settings " ^ (GeneralUtil.strlist2str settings_list) ^ " do not exist"))
		else
		    ()
	fun idToSetting id = 
	    case (List.find (fn(s)=> case s of 
					 FLAG (id', _) => id=id'
				       | SETTING (id', _) => id=id') (!settings)) of
		SOME s => s
	      | NONE => DynException.stdException("Can't find setting " ^ id, "DynamoOptions.settingsToJSON", Logger.INTERNAL)
		 
    in
	settingsListToJSON (map idToSetting exists)
    end
end
