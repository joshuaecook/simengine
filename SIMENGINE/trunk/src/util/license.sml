signature LICENSE =
sig

    (* the various forms of base packages we offer *)
    datatype product = SIMENGINE
    datatype class = TRIAL | BASIC | STANDARD | PROFESSIONAL | DEVELOPMENT

    (* feature listings that are encoded in the license file *)
    type enhancements = {}

    (* license info data type*)
    type license = {product: product,
		    key: int,
		    MaxMajorVersion: int,
		    MaxMinorVersion: int,
		    ExpirationDate: Date.date option,
		    Class: class,
		    Enhancements: enhancements}

    (* accessors/modifiers of license *)
    val set : license -> unit
    val get : unit -> license

    (* query license *)
    val isValidVersion : (int * int) -> bool
    val isExpired : unit -> bool
    val isTrial : unit -> bool
    val isBasic : unit -> bool
    val isStandard : unit -> bool
    val isProfessional : unit -> bool
    val isDevelopment : unit -> bool

    (* reporting functions *)
    val ClassToString : unit -> string


end
structure License =
struct

(* the various forms of base packages we offer *)
datatype product = SIMENGINE
datatype class = TRIAL | BASIC | STANDARD | PROFESSIONAL | DEVELOPMENT

type enhancements = {}

type license = {product: product,
		key: int,
		MaxMajorVersion: int,
		MaxMinorVersion: int,
		ExpirationDate: Date.date option,
		Class: class,
		Enhancements: enhancements}

(* define initial licenses internally *)
val development_license = {product=SIMENGINE,
			   key=0,
			   MaxMajorVersion=100,
			   MaxMinorVersion=100,
			   ExpirationDate=NONE,
			   Class=DEVELOPMENT,
			   Enhancements={}}

val basic_license = {product=SIMENGINE,
		     key=0,
		     MaxMajorVersion=1,
		     MaxMinorVersion=1,
		     ExpirationDate=NONE,
		     Class=BASIC,
		     Enhancements={}}

val default = basic_license

(* accessors and modifiers for current license *)
val currentLicense = ref default
fun get () = !currentLicense
fun set (license) = currentLicense := license

(* additional functions *)
fun ClassToString () =
    case (#Class (get())) of
	TRIAL => "Trial"
      | BASIC => "Basic"
      | STANDARD => "Standard"
      | PROFESSIONAL => "Professional"
      | DEVELOPMENT => "Development"

(* query functions *)
fun isTrial () = case (#Class (get())) of TRIAL => true | _ => false
fun isBasic () = case (#Class (get())) of BASIC => true | _ => false
fun isStandard () = case (#Class (get())) of STANDARD => true | _ => false
fun isProfessional () = case (#Class (get())) of PROFESSIONAL => true | _ => false
fun isDevelopment () = case (#Class (get())) of DEVELOPMENT => true | _ => false
    
(* testing function *)
fun isValidVersion (major, minor) = 
    let
	val major' = #MaxMajorVersion (get())
	val minor' = #MaxMinorVersion (get())
    in
	major <= major' andalso minor <= minor'
    end

(* look at the system clock and compare to make sure that the latest date isn't after the expiration of the trial. for a second test, make sure that the compile time is before the expiration time *)
fun isExpired () =
    let
	(* figure out what to compare against - the latest of the current date and the compile date *)
	val now = Date.fromTimeLocal (Time.now())
	val compile_date = Globals.compile_date
	val maximum_date = case Date.compare(now, compile_date) of
			       GREATER => now
			     | _ => compile_date

	val latest_date = #ExpirationDate (get())
    in
	case latest_date of
	    SOME date => (case Date.compare (maximum_date, date) of
			      GREATER => true
			    | _ => false)
	  | NONE => false
    end

end
