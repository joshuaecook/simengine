(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
signature CURRENT_LICENSE =
sig

(* query license *)
val findAndVerify : unit -> unit
val isTrial : unit -> bool
val isBasic : unit -> bool
val isStandard : unit -> bool
val isProfessional : unit -> bool
val isDevelopment : unit -> bool
val versionToString : unit -> string

end

structure CurrentLicense: CURRENT_LICENSE =
struct

structure L = License

val current = ref L.default

fun get () = ! current

local fun attr f () = f (get ())
in
val version = attr L.version
val product = attr L.product
val restriction = attr L.restriction
val maxMinorVersion = attr L.maxMinorVersion
val maxMajorVersion = attr L.maxMajorVersion
val expirationDate = attr L.expirationDate
val customerName = attr L.customerName
val customerOrganization = attr L.customerOrganization
end

fun versionToString () =
    case version () of
	L.TRIAL => "Trial"
      | L.BASIC => "Basic"
      | L.STANDARD => "Standard"
      | L.PROFESSIONAL => "Professional"
      | L.DEVELOPMENT => "Development"

fun licenseHolderToString () =
    let
	val name = customerName()
	val org = customerOrganization()
	val nameOrg = if org <> "" then
			  name ^ ", " ^ org
		      else
			  name
	val restr = restriction()
    in
	case restr of
	    L.SITE site => site
	  | L.HOSTID _ => nameOrg ^ " (single computer)"
	  | L.USERNAME user => nameOrg ^ " (user: " ^ user ^ ")"
	  | L.LICENSESERVER server => nameOrg ^ " (Floating license from server '" ^ server ^ "')"
    end

fun set license = (current := license; Globals.edition := (versionToString()); Globals.licenseHolder := (licenseHolderToString()))

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

fun isTrial () = 
    case (version ())
     of L.TRIAL => true | _ => false

(* Internal methods for managing and verifying current license *)
fun error message =
    (Logger.log_error (Printer.$ message);
     DynException.setErrored())

fun warning message =
    (Logger.log_warning (Printer.$ message))

fun defaultLicenseWarning message =
    let
	val _ = warning (message ^ "\nReverting to a Basic license.")
	val _ = set(L.default)
    in
	()
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

	val latest_date = expirationDate ()
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
	    val last_date = valOf (expirationDate ())
	    val str_date = Date.fmt "%B %d, %Y" last_date
	in
	    defaultLicenseWarning ("Software is valid until " ^ str_date ^ ". A new license is required to continue using some features of the software.")
	end
    else
	()

fun isValidVersion (major, minor) =
    let val (major', minor') = (maxMajorVersion (), maxMinorVersion ())
    in
	(major = major' andalso minor <= minor') orelse
	major < major'
    end

fun verifyValidVersion ver =
    if isValidVersion ver then
	()
    else
	let
	    val (cur_major, cur_minor) = ver
	    val cur = (Int.toString cur_major) ^ "." ^ (Int.toString cur_minor)
	    val (lic_major, lic_minor) = (maxMajorVersion (), maxMinorVersion ())
	    val lic = (Int.toString lic_major) ^ "." ^ (Int.toString lic_minor)					 
	in
	    defaultLicenseWarning ("Your software license is valid up until version " ^ lic ^ ".  This software is version " ^ cur ^ ".")
	end

fun verifyNotRestricted () =
    case (restriction ()) of
	L.USERNAME user => (case OS.Process.getEnv "USER" of 
			      SOME user' =>
			      if user = user' then
				  () (* passes *)
			      else
				  defaultLicenseWarning "This software is not licensed for the current user."
			    | NONE =>
			      defaultLicenseWarning "This software can not determine the current user of the system to validate license.")
			 
      | L.HOSTID mac_str => 
	(let
	     val (status, text) = Process.system("/sbin/ifconfig", ["-a"])
	     val matches = List.exists (fn(str)=>String.isSubstring mac_str (StdFun.toLower str)) text
	 in
	     if matches then
		 () (* found host id *)
	     else
		 defaultLicenseWarning "Machine is not licensed to run this version of the software."
	 end
	 handle Process.ProcessError => 
		defaultLicenseWarning ("Can not evaluate machine to verify that license is valid for this machine."))
				 
      | L.LICENSESERVER server => error "Network licensing is not currently supported in this version of the software"
      | L.SITE site => () (* site licenses are not restricted *)

fun findLicense () =
    let
	(* TODO - Error checking... Check for license in user directory? *)
	val licenseFileMain = OS.Path.concat (getSIMENGINE(), OS.Path.fromUnixPath "data/license.key")
	val licenseFileUser = OS.Path.concat (valOf (OS.Process.getEnv("HOME")), OS.Path.fromUnixPath ".simatra/license.key")

	fun readFile filename =
	    let val instream = TextIO.openIn filename
	    in TextIO.inputAll instream
	       before TextIO.closeIn instream
	    end

	val license = License.licenseFromData(readFile licenseFileMain)
	    handle _ => License.licenseFromData(readFile licenseFileUser)
	    handle _ => L.default
    in
	set(license)
    end

fun findAndVerify () =
    let
	val _ = findLicense ()
	(* verify that it is not expired *)
	val _ = verifyExpired ()
	(* now verify that the version is correct *)
	val _ = verifyValidVersion (BuildOptions.majorVersion, BuildOptions.minorVersion)
	(* check to make sure that there the user/hostid/network/site specification is correct *)
	val _ = verifyNotRestricted ()
    in
	()
    end

end
