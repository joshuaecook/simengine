structure Globals =
struct

(* global status *)
val eof_encountered = ref false

val core_init = ref true

val lastline_yypos = ref 0
val line_count = ref 0

(* this type is required because new() does not return enough information to create a properly sized reference *)
val base_env: (KEC.exp Env.env ref * (KEC.exp Env.env * KEC.exp Env.env * KEC.exp Env.env option ref) * PosLog.pos list) ref 
  = ref (ref (Env.new()), (Env.new(), Env.new(), ref NONE), PosLog.new())



(* global constants *)
val simatra_url = "http://www.simatratechnologies.com"

fun CurrentDateTime () = (Date.fromTimeLocal (Posix.ProcEnv.time ())) : Date.date
val compile_date = CurrentDateTime()
val year = Date.year (CurrentDateTime())

val copyright = "Copyright "^(Int.toString year)^" Simatra Modeling Technologies, L.L.C."

val short_name = "simEngine"
val name = short_name ^ " Dynamical System Compiler"
(*val version = BuildOptions.version*)
val version = (GeneralUtil.int2str BuildOptions.majorVersion) ^ "." ^ (GeneralUtil.int2str BuildOptions.minorVersion) ^ BuildOptions.versionRevision
val extension = ".dso"
val edition = ref "N/A"
val licenseHolder = ref "N/A"
val expirationString = ref ""
val daysToString = ref (fn(d:Date.date)=>"")

fun buildDateAsDate () = 
    let
	val days = Int.toLarge (BuildOptions.buildDate)
	val seconds = days * 24 * 3600
    in
	(Date.fromTimeUniv o Time.fromReal o Real.fromLargeInt) seconds
    end    

fun buildDate () = (!daysToString) (buildDateAsDate())

fun startupMessage () =
    (name ^ " v" ^ version ^ ", " ^ (!edition) ^ " Edition" ^ "\n"
     ^ (!licenseHolder) ^ " " ^ (!expirationString) ^ "\n"
     ^ "[built: " ^ (buildDate()) ^ "]" ^ (if BuildOptions.build = "unknown" then "" else (" " ^ BuildOptions.build)) ^ "\n"
     ^ copyright)
    

val path = ref let
 	       val env = Posix.ProcEnv.environ()
	       val pathstr = case List.find (String.isPrefix "PATH") env of
				 NONE => (DynException.stdException ("Host's environment does not contain a path", "Globals.path", Logger.DATA))
			       | SOME p => p
	       val paths = List.nth(String.fields(fn(c) => c = #"=") pathstr, 1)
	   in
	       String.fields(fn(c) => c = #":") paths  
	   end


end
