structure Manifest = struct

val VERSION: int = 0

datatype manifest
  = M of {creationDate: Time.time,
	  dolFilename: string,
	  dslFilenames: string list,
	  environment: (string * string) list,
	  executables: executable list,
	  version: int}

     and executable 
       = EXE of {debug: bool,
		 cSourceFilename: string,
		 precision: DOF.precisiontype,
		 profile: bool,
		 target: Target.target}


local
    fun access f (M r) = f r
in
val creationDate = access #creationDate
val dolFilename = access #dolFilename
val dslFilenames = access #dslFilenames
val environment = access #environment
val executables = access #executables
val version = access #version
end


fun new {dolFilename, dslFilenames, environment, executables} =
    M {creationDate = Time.now (),
       dolFilename = dolFilename,
       dslFilenames = dslFilenames,
       environment = environment,
       executables = executables,
       version = VERSION}


local 
    open JSON

    val targetToJSON =
     fn Target.CPU => string "CPU"
      | Target.OPENMP => string "OPENMP"
      | Target.CUDA => string "CUDA"

    fun targetFromJSON json =
	case stringVal json
	 of "CPU" => Target.CPU
	  | "OPENMP" => Target.OPENMP
	  | "CUDA" => Target.CUDA
	  | _ => raise Option

    fun executableToJSON (EXE {debug, cSourceFilename,precision, profile, target}) =
	JSON.object [("debug", bool debug),
		     ("cSourceFilename", string cSourceFilename),
		     ("precision", string (case precision of DOF.DOUBLE => "DOUBLE" | DOF.SINGLE => "SINGLE")),
		     ("profile", bool profile),
		     ("target", targetToJSON target)]

    fun executableFromJSON json =
	if isObject json then
	    EXE {debug = memberVal (json, "debug", boolVal),
		 cSourceFilename = memberVal (json, "cSourceFilename", stringVal),
		 precision = case memberVal (json, "precision", stringVal)
			      of "DOUBLE" => DOF.DOUBLE | "SINGLE" => DOF.SINGLE | _ => raise Option,
		 profile = memberVal (json, "profile", boolVal),
		 target = memberVal (json, "target", targetFromJSON)}
	else raise Option


in
fun toJSON m =
    JSON.object [("creationDate", (int o Time.toSeconds o creationDate) m),
		 ("dolFilename", (string o dolFilename) m),
		 ("dslFilenames", (array o (map string) o dslFilenames) m),
		 ("environment", (object o (map (fn (k, v) => (k, string v))) o environment) m),
		 ("executables", (array o (map executableToJSON) o executables) m),
		 ("version", (int o IntInf.fromInt o version) m)]
    
fun fromJSON json =
    if isObject json then
	M {creationDate = memberVal (json, "creationDate", Time.fromSeconds o intVal),
	   dolFilename = memberVal (json, "dolFilename", stringVal),
	   dslFilenames = memberVal (json, "dslFilenames", (map stringVal) o valOf o elements),
	   environment = memberVal (json, "environment", (map (fn (k, v) => (k, stringVal v))) o valOf o members),
	   executables = memberVal (json, "executables", (map executableFromJSON) o valOf o elements),
	   version = memberVal (json, "version", IntInf.toInt o intVal)}
    else raise Option
end

end (* structure Manifest *)
