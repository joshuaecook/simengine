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
		 target: Target.target,
		 precision: DOF.precisiontype,
		 profile: bool}


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
in
fun toJSON m =
    JSON.object [("creationDate", (int o Time.toSeconds o creationDate) m),
		 ("dolFilename", (string o dolFilename) m),
		 ("dslFilenames", (array o (map string) o dslFilenames) m),
		 ("environment", (object o (map (fn (k, v) => (k, string v))) o environment) m),
		 ("version", (int o IntInf.fromInt o version) m)]
    
fun fromJSON json =
    if isObject json then
	M {creationDate = Time.fromSeconds (memberVal (json, "creationDate", intVal)),
	   dolFilename = memberVal (json, "dolFilename", stringVal),
	   dslFilenames = memberVal (json, "dslFilenames", (map stringVal) o valOf o elements),
	   environment = nil,
	   executables = nil,
	   version = IntInf.toInt (memberVal (json, "version", intVal))}
    else raise Option
end

end (* structure Manifest *)
