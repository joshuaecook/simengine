signature LICENSE =
sig

    (* customer information encoded in the license *)
    type customer = {id: int,
		     name: string,
		     organization: string}

    (* the various forms of base packages we offer *)
    datatype product = SIMENGINE
    datatype class = TRIAL | BASIC | STANDARD | PROFESSIONAL | DEVELOPMENT

    (* we can support various licensing modes - this has to be defined in the license file *)
    type byte = Word8.word
    type mac_address = (byte * byte * byte * byte * byte * byte)
    datatype restriction = USERNAME of string       (* restricted to a particular user only *)
			 | HOSTID of mac_address    (* restricted to a particular host id (mac address) *)
			 | LICENSESERVER of string  (* network server which can be either an ip address or a dns name (NOT SUPPORTED) *)
			 | SITE of string           (* site license - can be used with no restrictions *)

    (* feature listings that are encoded in the license file *)
    type enhancements = {}

    (* license info data type*)
    type license = {product: product,
		    customer: customer,
		    restriction: restriction,
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

    (* verify that the license is valid by checking the restriction, returning an error if the restriction is not met *)
    val verifyNotRestricted : unit -> unit
    val verifyValidVersion : (int * int) -> unit
    val verifyExpired : unit -> unit

    (* reporting functions *)
    val ClassToString : unit -> string

    (* default license *)
    val default : license

end
structure License : LICENSE =
struct

(* customer information encoded in the license *)
type customer = {id: int,
		 name: string,
		 organization: string}

(* the various forms of base packages we offer *)
datatype product = SIMENGINE
datatype class = TRIAL | BASIC | STANDARD | PROFESSIONAL | DEVELOPMENT

type byte = Word8.word
type mac_address = (byte * byte * byte * byte * byte * byte)
datatype restriction = USERNAME of string
		     | HOSTID of mac_address
		     | LICENSESERVER of string
		     | SITE of string

type enhancements = {}

type license = {product: product,
		customer: customer,
		restriction: restriction,
		key: int,
		MaxMajorVersion: int,
		MaxMinorVersion: int,
		ExpirationDate: Date.date option,
		Class: class,
		Enhancements: enhancements}

(* define initial licenses internally *)
val development_license = {product=SIMENGINE,
			   customer={id=999999,
				     name="Simatra Developer",
				     organization="Simatra"},
			   restriction=SITE "Simatra HQ",
			   key=0,
			   MaxMajorVersion=100,
			   MaxMinorVersion=100,
			   ExpirationDate=NONE,
			   Class=DEVELOPMENT,
			   Enhancements={}}

val i2s = Util.i2s

fun error message =
    (Logger.log_error (Printer.$ message);
     DynException.setErrored())
	
fun genMacAddress str = 
    let
	fun err () = error ("Invalid mac address <"^str^"> passed")
	val str_bytes = String.tokens (fn(c)=> c = #":") str
	val words = List.mapPartial Word8.fromString str_bytes
    in
	case words of
	    a::b::c::d::e::f::nil => (a, b, c, d, e, f)
	  | _ => (err();
		  genMacAddress "00:00:00:00:00:00")
    end

fun macAddressToString (a, b, c, d, e, f) =
    let
	val str = [Word8.toString a,
		   Word8.toString b,
		   Word8.toString c,
		   Word8.toString d,
		   Word8.toString e,
		   Word8.toString f]
    in
	StdFun.toLower (String.concatWith ":" str)
    end

val hostid_license : license = 
    {product=SIMENGINE,
     customer={id=100000,
	       name="Free User",
	       organization="Personal"},
     restriction=HOSTID (genMacAddress "00:24:36:b6:d0:7d"), (* for testing on Franklin *)
     key=0,
     MaxMajorVersion=1,
     MaxMinorVersion=1,
     ExpirationDate=NONE,
     Class=BASIC,
     Enhancements={}}

val user_license : license = 
    {product=SIMENGINE,
     customer={id=100000,
	       name="Free User",
	       organization="Personal"},
     restriction=USERNAME "qa", (* have to set USER to make it work *)
     key=0,
     MaxMajorVersion=1,
     MaxMinorVersion=1,
     ExpirationDate=NONE,
     Class=BASIC,
     Enhancements={}}

val basic_license : license = 
    {product=SIMENGINE,
     customer={id=100000,
	       name="Free User",
	       organization="Personal"},
     restriction=SITE "Anywhere",
     key=0,
     MaxMajorVersion=1,
     MaxMinorVersion=1,
     ExpirationDate=NONE,
     Class=BASIC,
     Enhancements={}}

val development_license : license = 
    {product=SIMENGINE,
     customer={id=100000,
	       name="Simatra Developer",
	       organization="Simatra HQ"},
     restriction=SITE "Simatra",
     key=0,
     MaxMajorVersion=1,
     MaxMinorVersion=1,
     ExpirationDate=NONE,
     Class=DEVELOPMENT,
     Enhancements={}}

(* CHANGE HERE - by default, we shouldn't have an unrestricted license *)
val default = development_license

(* accessors and modifiers for current license *)
val currentLicense = ref default
fun get () = !currentLicense

fun ClassToString () =
    case (#Class (get())) of
	TRIAL => "Trial"
      | BASIC => "Basic"
      | STANDARD => "Standard"
      | PROFESSIONAL => "Professional"
      | DEVELOPMENT => "Development"

fun set (license) = (currentLicense := license;
		     Globals.edition := (ClassToString()))
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

fun verifyValidVersion ver =
    if isValidVersion ver then
	()
    else
	let
	    val (cur_major, cur_minor) = ver
	    val cur = (i2s cur_major) ^ "." ^ (i2s cur_minor)
	    val (lic_major, lic_minor) = (#MaxMajorVersion (get()), #MaxMinorVersion (get()))
	    val lic = (i2s lic_major) ^ "." ^ (i2s lic_minor)					 
	in
	    error ("Your software license is valid up until version " ^ lic ^ ".  This software is version " ^ cur ^ ".")
	end

(* look at the system clock and compare to make sure that the latest date isn't after the expiration of the trial. for a second test, make sure that the compile time is before the expiration time *)
fun isExpired () =
    let
	(* figure out what to compare against - the latest of the current date and the compile date *)
	val now = Date.fromTimeLocal (Time.now())
	val compile_date = case Date.fromString (BuildOptions.buildDate) of
			       SOME date => date
			     | NONE => now (* otherwise, if we can't read it, no worries, just use the now time *)

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

fun verifyExpired () =
    if isExpired () then
	let
	    val last_date = valOf (#ExpirationDate (get()))
	    val str_date = Date.fmt "%B %d, %Y" last_date
	in
	    error ("Software is valid until " ^ str_date ^ ". A new license is required to continue using the software.")
	end
    else
	()

(* this will look at the restriction field to make sure that the license is valid for the system/user *)
local
in
fun verifyNotRestricted () =
    case (#restriction (get())) of
	USERNAME user => (case OS.Process.getEnv "USER" of 
			      SOME user' =>
			      if user = user' then
				  () (* passes *)
			      else
				  error "This software is not licensed for the current user"
			    | NONE => error "This software can not determine the current user of the system to validate license")
      | HOSTID mac_addr => 
	let
	    val mac_str = macAddressToString mac_addr
	    val (status, text) = Process.system("/sbin/ifconfig", ["-a"])
		handle Process.ProcessError => (error "Can not evaluate machine to verify that license is valid";(~1, []))
				 
	    val matches = List.exists (fn(str)=>String.isSubstring mac_str (StdFun.toLower str)) text
	in
	    if matches then
		() (* found host id *)
	    else
		error "Machine is not licensed to run this version of the software"
	end
      | LICENSESERVER server => error "Network licensing is not currently supported in this version of the software"
      | SITE site => () (* site licenses are not restricted *)

end

end

(* set as default *)
val _ = License.set(License.default)

