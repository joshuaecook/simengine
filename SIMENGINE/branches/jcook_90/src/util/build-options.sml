signature BUILD_OPTIONS = sig
val allowSWBackend: bool
val allowFPBackend: bool
val allowHWBackend: bool

val build: string
val buildDate: string
val buildTime: Time.time

val version: string
val devVersion: bool

end

structure BuildOptions:> BUILD_OPTIONS = struct

fun invalid name = raise Fail ("Build options contains invalid data for " ^ name)

local val optionsFile = case OS.Process.getEnv("SIMENGINE")
			 of SOME path => OS.Path.mkAbsolute {path=OS.Path.fromUnixPath "data/build-options.json",
							     relativeTo=path}
			  | NONE => raise Fail ("Environment variable SIMENGINE must be set to locate the build options")
in
val options = ParseJSON.parseFile optionsFile
val _ = if JSON.isObject options then ()
	else invalid "all"
end

val allowSWBackend =
    JSON.boolVal (JSON.memberDefault (options, "allowSWBackend", {default=JSON.bool true}))

val allowFPBackend =
    JSON.boolVal (JSON.memberDefault (options, "allowFPBackend", {default=JSON.bool false}))

val allowHWBackend =
    JSON.boolVal (JSON.memberDefault (options, "allowHWBackend", {default=JSON.bool false}))

val build =
    case JSON.memberValue (options, "build", JSON.toString)
     of SOME s => s | _ => invalid "build"

val buildDate =
    case JSON.memberValue (options, "buildDate", JSON.toString)
     of SOME s => s | _ => invalid "buildDate"

val buildTime =
    case JSON.memberValue (options, "buildTime", JSON.toInt)
     of SOME z => Time.fromSeconds (IntInf.toLarge z) | _ => invalid "buildTime"

val version =
    case JSON.memberValue (options, "buildDate", JSON.toString)
     of SOME s => s | _ => invalid "buildDate"

val devVersion =
    JSON.boolVal (JSON.memberDefault (options, "devVersion", {default=JSON.bool false}))

end
