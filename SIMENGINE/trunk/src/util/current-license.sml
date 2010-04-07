(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
signature CURRENT_LICENSE =
sig

(* accessor/modifier *)
val get: unit -> License.license
val set: License.license -> unit

(* query license *)
val findAndVerify : unit -> unit
val isTrial : unit -> bool
val isBasic : unit -> bool
val isStandard : unit -> bool
val isProfessional : unit -> bool
val isDevelopment : unit -> bool
val expirationDate : unit -> Date.date option
val customerName: unit -> string
val customerOrganization: unit -> string


(* check if update to simEngine software is valid *)
val validateUpdate : int -> order option

(* datatype for license errors *)
datatype license_error = EXPIRED of string
		       | OUTOFMAINTENANCE of string
		       | INVALIDVERSION of {cur_ver: string, lic_ver: string}
		       | WRONGUSER of {cur_user: string, lic_user: string, lic_name: string, lic_organization: string} option
		       | WRONGMACHINE of machine_error
		       | NETWORKNOTSUPPORTED

and machine_error = CANTVERIFYHOST 
		  | WRONGHOST

val versionToString : unit -> string

(* Test a license externally *)
val checkLicense : License.license option -> license_error option
val licensingErrorToID : license_error -> string

end

structure CurrentLicense: CURRENT_LICENSE =
struct

(* datatype for license errors *)
datatype license_error = EXPIRED of string
		       | OUTOFMAINTENANCE of string
		       | INVALIDVERSION of {cur_ver: string, lic_ver: string}
		       | WRONGUSER of {cur_user: string, lic_user: string, lic_name: string, lic_organization: string} option
		       | WRONGMACHINE of machine_error
		       | NETWORKNOTSUPPORTED

and machine_error = CANTVERIFYHOST 
		  | WRONGHOST

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

fun expirationToString () = 
    let
	val isTrial = case version() of L.TRIAL => true | _ => false
	val doesExpire = case version() of L.TRIAL => false | L.BASIC => false | _ => true
	val expDate = expirationDate()
	val numDaysTillExpiration = case expDate of
					SOME d => Util.daysFromDate d
				      | NONE => ~1
    in
	case expDate of
	    SOME d => if isTrial then
			  if numDaysTillExpiration < 0 then
			      "" (* reverting to free version since expired *)
			  else if numDaysTillExpiration = 0 then
			      "(Trial license expiring today)"
			  else 
			      "(Trial license expiring in "^(Util.infint2str numDaysTillExpiration)^" days)"
		      else if doesExpire andalso (isSome expDate) then (* this is for maintenance *)
			  if numDaysTillExpiration < 0 then
			      "(Maintenance for the "^(versionToString())^" edition has expired)"
			  else if numDaysTillExpiration = 0 then
			      "(Maintenance for the "^(versionToString())^" edition expiring today)"
			  else
			      "(Maintenance available until "^(Util.daysToString d)^")"
		      else
			  "" (* not sure what to do here since all licenses should expire *)
	  | NONE => (* could be a basic license that does not expire *)
	    ""
    end


val verifyLicenseToJSONCallBack = ref (fn(_)=>JSON.null)    
fun set license = (current := license; 
		   Globals.edition := (versionToString()); 
		   Globals.licenseHolder := (licenseHolderToString()); 
		   Globals.expirationString := (expirationToString());
		   L.verifyLicenseToJSONCallback(!verifyLicenseToJSONCallBack))

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
	val _ = warning (message ^ "\nReverting from a "^(versionToString())^" to a Basic license.")
	val _ = set(L.default)
    in
	()
    end

(* look at the system clock and compare to make sure that the latest date isn't after the expiration of the trial. for a second test, make sure that the compile time is before the expiration time *)
fun isExpired () =
    if isTrial() then (* only the trial can be expired *)
	let
	    (* figure out what to compare against - the latest of the current date and the compile date *)
	    val now = Date.fromTimeLocal (Time.now())
	    val compile_date = Globals.buildDateAsDate() 

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
    else
	false

fun verifyExpired () =
    if isExpired () then
	let
	    val last_date = valOf (expirationDate ())
	    val str_date = Date.fmt "%B %d, %Y" last_date
	in
	    SOME (EXPIRED str_date)
	end
    else
	NONE


datatype maintenanceUseCase = EXECUTION | UPGRADE
(* look at the system clock and compare to make sure that the latest date isn't after the expiration of the trial. for a second test, make sure that the compile time is before the expiration time *)
fun isOutOfMaintenance (usecase) =
    if isStandard() orelse isProfessional() then (* only the trial can be expired *)
	let
	    (* figure out what to compare against - the latest of the current date and the compile date *)
	    val now = Date.fromTimeLocal (Time.now())
	    val compile_date = Globals.buildDateAsDate() 

	    val maximum_date = case usecase of
				   EXECUTION => compile_date
				 | UPGRADE => 		
				   case Date.compare(now, compile_date) of
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
    else
	false

fun verifyOutOfMaintenance (usecase) =
    if isOutOfMaintenance (usecase) then
	let
	    val last_date = valOf (expirationDate ())
	    val str_date = Date.fmt "%B %d, %Y" last_date
	in
	    SOME (OUTOFMAINTENANCE str_date)
	end
    else
	NONE

fun isValidVersion (major, minor) =
    let val (major', minor') = (maxMajorVersion (), maxMinorVersion ())
    in
	(major = major' andalso minor <= minor') orelse
	major < major'
    end

fun verifyValidVersion ver =
    if isValidVersion ver then
	NONE
    else
	let
	    val (cur_major, cur_minor) = ver
	    val cur = (Int.toString cur_major) ^ "." ^ (Int.toString cur_minor)
	    val (lic_major, lic_minor) = (maxMajorVersion (), maxMinorVersion ())
	    val lic = (Int.toString lic_major) ^ "." ^ (Int.toString lic_minor)					 
	in
	    SOME (INVALIDVERSION {cur_ver=cur, lic_ver=lic})
	end

fun verifyNotRestricted () : license_error option =
    case (restriction ()) of
	L.USERNAME user => (case OS.Process.getEnv "USER" of 
			      SOME user' =>
			      if user = user' then
				  NONE (* passes *)
			      else
				  SOME (WRONGUSER (SOME {lic_name=customerName(), 
							 lic_organization=customerOrganization(),
							 lic_user=user,
							 cur_user=user'}))
			    | NONE =>
			      SOME (WRONGUSER NONE))
			 
      | L.HOSTID mac_str => 
	(let
	     val (status, text) = Process.system("/sbin/ifconfig", ["-a"])
	     val matches = List.exists (fn(str)=>String.isSubstring (StdFun.toLower mac_str) (StdFun.toLower str)) text
	 in
	     if matches then
		 NONE (* found host id *)
	     else
		 SOME (WRONGMACHINE WRONGHOST)
	 end
	 handle Process.ProcessError => 
		SOME (WRONGMACHINE CANTVERIFYHOST))
				 
      | L.LICENSESERVER server => SOME NETWORKNOTSUPPORTED
      | L.SITE site => NONE (* site licenses are not restricted *)

fun licenseFromFile filename =
    let
	fun readFile filename =
	    let val instream = TextIO.openIn filename
	    in SOME (TextIO.inputAll instream)
	       before TextIO.closeIn instream
	    end
	    handle IO.Io _ => NONE
    in
	case readFile filename of
	    SOME data => License.licenseFromData data
	  | NONE => NONE
    end

fun findLicense () =
    let
	(* licenseFile can be either an empty string or contain a filename *)
	val lic_file = DynamoOptions.getStringSetting "licenseFile"
	(*val _ = Util.log("Command line license file: " ^ lic_file)*)
	val licenseCmdLine = if lic_file = "" then
				 NONE
			     else if Directory.isFile lic_file then
				 SOME lic_file
			     else
				 (Logger.log_error (Printer.$("Can not open license file '"^lic_file^"'"));
				  DynException.setErrored();
				  DynException.checkToProceed();
				  NONE)
	(*val _ = Util.log(case licenseCmdLine of SOME lic => "Found file" | NONE => "Did not find file")*)
				 
	val licenseFileMain = OS.Path.concat (getSIMENGINE(), OS.Path.fromUnixPath "data/license.key")
	val licenseFileUser = OS.Path.concat (valOf (OS.Process.getEnv("HOME")), OS.Path.fromUnixPath ".simatra/license.key")

	val license = case licenseCmdLine of
			  (* here we want to use the one passed in *)
			  SOME file => (case licenseFromFile file of
					    SOME lic => lic
					  | NONE => (Logger.log_data_error file (Printer.$("Can not process license file '"^lic_file^"'"));
						     DynException.setErrored();
						     DynException.checkToProceed();
						     L.default))
			(* otherwise go through the default search path *)
			| NONE => (case licenseFromFile licenseFileMain of
				       SOME lic => lic
				     | NONE => (case licenseFromFile licenseFileUser of
						    SOME lic => lic
						  (* finally, can't find anything, so just use the default basic license *)
						  | NONE => L.default))    
    in
	set(license)
    end


val fmt = String.concatWith ":"
fun licensingErrorToID (EXPIRED str_date) = fmt ["EXPIRED", str_date]
  | licensingErrorToID (OUTOFMAINTENANCE str_date) = fmt ["OUTOFMAINTENANCE", str_date]
  | licensingErrorToID (INVALIDVERSION {cur_ver, lic_ver}) = fmt ["INVALIDVERSION", cur_ver, lic_ver]
  | licensingErrorToID (WRONGUSER NONE) = fmt ["WRONGUSER",""]
  | licensingErrorToID (WRONGUSER (SOME {cur_user, lic_user, lic_name, lic_organization})) = fmt ["WRONGUSER", cur_user]
  | licensingErrorToID (WRONGMACHINE WRONGHOST) = fmt ["WRONGMACHINE", "WRONGHOST"]
  | licensingErrorToID (WRONGMACHINE CANTVERIFYHOST) = fmt ["WRONGMACHINE", "CANTVERIFYHOST"]
  | licensingErrorToID (NETWORKNOTSUPPORTED) = fmt ["NETWORKNOTSUPPORTED"]


fun licensingErrorToString (EXPIRED str_date) = ("Software was valid until " ^ str_date ^ ". A new license is required to continue using some features of the software.")
  | licensingErrorToString (OUTOFMAINTENANCE str_date) = ("Software is out of maintenance and can not be updated after " ^ str_date ^ ". A new license is required to upgrade this software.")
  | licensingErrorToString (INVALIDVERSION {cur_ver, lic_ver}) = ("Your software license is valid up until version " ^ lic_ver ^ ".  This software is version " ^ cur_ver ^ ".")
  | licensingErrorToString (WRONGUSER (SOME {cur_user, lic_user, lic_name, lic_organization})) = ("This software is licensed to " ^ (lic_name) ^ "," ^ (lic_organization) ^ " (" ^ lic_user ^ ") and cannot be run by the current user (" ^ cur_user ^ ").")
  | licensingErrorToString (WRONGUSER NONE) = ("This software can not determine the current user of the system to validate license.")
  | licensingErrorToString (WRONGMACHINE WRONGHOST) = ("Machine is not licensed to run this version of the software.")
  | licensingErrorToString (WRONGMACHINE CANTVERIFYHOST) = ("Can not evaluate machine to verify that license is valid for this machine.")
  | licensingErrorToString NETWORKNOTSUPPORTED = ("Network licensing is not currently supported in this version of the software")

local
    open JSON
in
fun licensingErrorToJSON NONE = bool true
  | licensingErrorToJSON (SOME err) =
    case err of
	EXPIRED str_date => object [("status", string "expired"),
				    ("date", string str_date)]
      | OUTOFMAINTENANCE str_date => object [("status", string "outofmaintenance"),
					     ("date", string str_date)]
      | INVALIDVERSION {cur_ver, lic_ver} => object [("status", string "invalidversion"),
						     ("cur_ver", string cur_ver),
						     ("lic_ver", string lic_ver)]
      | WRONGUSER NONE => object [("status", string "wronguser"),
				  ("queried", bool false)]
      | WRONGUSER (SOME {cur_user, lic_user, lic_name, lic_organization}) => 
	object [("status", string "wronguser"),
		("queried", bool true),
		("cur_user", string cur_user),
		("lic_user", string lic_user),
		("lic_name", string lic_name),
		("lic_organization", string lic_organization)]
      | WRONGMACHINE CANTVERIFYHOST => object [("status", string "wrongmachine"),
					   ("queried", bool false)]
      | WRONGMACHINE WRONGHOST => object [("status", string "wrongmachine"),
					  ("queried", bool true)]
      | NETWORKNOTSUPPORTED => object [("status", string "networknotsupported")]
end


val logLicensingErrors  = defaultLicenseWarning o licensingErrorToString

fun findAndVerify () =
    let
	val _ = findLicense ()
	(* verify that it is not expired *)
	val _ = case verifyExpired () of
		    SOME err => logLicensingErrors err
		  | NONE => ()
	(* verify that it is not out of maintenance *)
	val _ = case verifyOutOfMaintenance (EXECUTION) of
		    SOME err => logLicensingErrors err
		  | NONE => ()
	(* now verify that the version is correct *)
	val _ = case verifyValidVersion (BuildOptions.majorVersion, BuildOptions.minorVersion) of
		    SOME err => logLicensingErrors err
		  | NONE => ()
	(* check to make sure that there the user/hostid/network/site specification is correct *)
	val _ = case verifyNotRestricted () of
		    SOME err => logLicensingErrors err
		  | NONE => ()
    in
	()
    end

(* checkLicense will verify if a license is valid or not.  It will not return any messages to the screen, 
 * instead, it will return an option result.  If NONE, the license is valid, otherwise SOME license_error is 
 * returned.  The input is a license option.  If NONE is passed in, the current license is checked, otherwise 
 * the license passed in is validated. *)
fun checkLicense (SOME license) =
    let
	(* save the current license *)
	val prev_license = get()

	(* now set the license *)
	val _ = set(license)

	(* run checkLicense with the currently set license *)
	val result = checkLicense NONE
		     handle e => (set(prev_license);
				  raise e)

	(* restore the previous license *)
	val _ = set(prev_license)
    in
	result
    end
  | checkLicense NONE =
    (* run the verification checks *)
    case verifyExpired() of
	SOME err => SOME err
      | NONE => (case verifyOutOfMaintenance(UPGRADE) of
		     SOME err => SOME err
		   | NONE => (case verifyValidVersion (BuildOptions.majorVersion, BuildOptions.minorVersion) of
				  SOME err => SOME err
				| NONE => (case verifyNotRestricted () of
					       SOME err => SOME err
					     | NONE => NONE)))
		
(* This callback will be used by the license code to produce JSON data of the status of a license *)
val _ = (verifyLicenseToJSONCallBack) := (fn(license)=>(licensingErrorToJSON o checkLicense o SOME) license)

(* validateUpdate is passed release info and the result is that the update is a GREATER, EQUAL, or LESS order. If the result is NONE, then the update is not valid. *)
fun validateUpdate days =
    let
	val cur_date = BuildOptions.buildDate
	val order = if days > cur_date then GREATER
		    else if days < cur_date then LESS
		    else EQUAL

	val valid = if isTrial() orelse isBasic() then
			true
		    else
			let
			    val days = Int.toLarge days
			    val seconds : IntInf.int = days * 24 * 3600
			    val updateDate = (Date.fromTimeUniv o Time.fromReal o Real.fromLargeInt) seconds
			in
			    case expirationDate () of
				SOME expirationDate => 
				(case Date.compare (updateDate, expirationDate) of
				     GREATER => false (* don't allow updates past the expiration date *)
				   | _ => true)
			      | NONE => true (* if it doesn't expire, then the update is valid *)
			end
    in
	if valid then
	    SOME order
	else
	    NONE
    end
				    

end
