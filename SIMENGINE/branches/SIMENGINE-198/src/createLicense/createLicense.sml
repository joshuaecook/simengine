
exception InvalidLicenseGenerationData of string

val keyvals = Cgi.keyValues()

val secondsPerDay = 60*60*24

fun makeLicenseData () =
    let
	fun getVal key =
	    #2 (hd (List.filter (fn s => (#1 s) = key) keyvals))
	    handle _ => raise InvalidLicenseGenerationData ("Key " ^ key ^ " not found!")
	fun getIntVal key =
	    valOf ((StringCvt.scanString (Int.scan StringCvt.DEC))(getVal key))
	    handle _ => raise InvalidLicenseGenerationData ("Key " ^ key ^ " not found!")

	val customerID = getIntVal "customerID"
	val customerName = getVal "customerName"
	val customerOrganization = getVal "customerOrganization"
	val restriction = 
	    let
		val restrictionCode = getIntVal "restriction"
		val restrictionString = getVal "restrictionString"
	    in
		if restrictionCode = 0 then License.USERNAME restrictionString
		else if restrictionCode = 1 then License.HOSTID restrictionString
		else if restrictionCode = 2 then License.LICENSESERVER restrictionString
		else if restrictionCode = 3 then License.SITE restrictionString
		else raise InvalidLicenseGenerationData ("Invalid restriction code: " ^ (Int.toString restrictionCode))
	    end
	val serialNumber = getIntVal "serialNumber"
	val maxMajorVersion = getIntVal "maxMajorVersion"
	val maxMinorVersion = getIntVal "maxMinorVersion"
	val date =
	    let
		val dateVal = (getIntVal "daysToExpiration") * secondsPerDay
	    in
		if dateVal = 0 then NONE
		else SOME (Date.fromTimeLocal (Time.+(Time.fromSeconds(IntInf.fromInt dateVal), Time.now())))
	    end
	val versionCode = getIntVal "versionCode"
	val version = if versionCode = 0 then License.BASIC
		      else if versionCode = 1 then License.STANDARD
		      else if versionCode = 2 then License.PROFESSIONAL
		      else if versionCode = 3 then License.TRIAL
		      else if versionCode = 4 then License.DEVELOPMENT
		      else raise InvalidLicenseGenerationData ("Invalid version code: " ^ (Int.toString versionCode))
				 
	val license =
	    License.make {product=License.SIMENGINE,
			  customerID=customerID,
			  customerName=customerName,
			  customerOrganization=customerOrganization,
			  restriction=restriction,
			  serialNumber=serialNumber,
			  maxMajorVersion=maxMajorVersion,
			  maxMinorVersion=maxMinorVersion,
			  expirationDate=date,
			  version=version,
			  enhancements={}}
    in
	License.licenseToData(license)
    end
    handle InvalidLicenseGenerationData error => raise InvalidLicenseGenerationData error
	 | Cgi.NoCgiInterface error => raise InvalidLicenseGenerationData error
	 | Fail error => raise InvalidLicenseGenerationData error
	 | _ => raise InvalidLicenseGenerationData "Unknown License Error!"

val licenseData = "<pre>" ^ (makeLicenseData ()) ^ "</pre>\n"
    handle InvalidLicenseGenerationData error => error
val htmlHeader = "Content-type: text/html\n\n<html><body>\n"
val htmlFooter = "</body></html>"
fun out s = TextIO.output(TextIO.stdOut, s)

val _ = out htmlHeader
val _ = out licenseData
(* val _ = map (out o (fn (k,v) => k ^ " = " ^ v ^ "<br />")) keyvals *)
val _ = out htmlFooter
