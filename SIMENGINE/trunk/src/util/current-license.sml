(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
signature CURRENT_LICENSE =
sig

val set: License.license -> unit
val get: unit -> License.license

(* query license *)
val isValidVersion : (int * int) -> bool
val isExpired : unit -> bool
val isTrial : unit -> bool
val isBasic : unit -> bool
val isStandard : unit -> bool
val isProfessional : unit -> bool
val isDevelopment : unit -> bool


end

structure CurrentLicense: CURRENT_LICENSE =
struct

struct L = License


val current = ref L.default
fun get () = ! current
fun set license = current := license


local fun attr f () = f (get ())
in
val version = attr L.version
val product = attr L.product
val restriction = attr L.restriction
val maxMinorVersion = attr L.maxMinorVersion
val maxMajorVersion = attr L.maxMajorVersion
end

fun isValidVersion (major, minor) =
    let val (major', minor') = (maxMajorVersion (), maxMinorVersion ())
    in
	(major == major' andalso minor <= minor') orelse
	major < major'
    end

fun isTrial () = 
    case version ()
     of L.TRIAL => true | _ => false

fun isBasic () = 
    case version ()
     of L.BASIC => true | _ => false

fun isStandard () = 
    case version ()
     of L.STANDARD => true | _ => false

fun isProfessional () = 
    case version ()
     of L.PROFESSIONAL => true | _ => false

fun isDevelopment () = 
    case version ()
     of L.DEVELOPMENT => true | _ => false


end
