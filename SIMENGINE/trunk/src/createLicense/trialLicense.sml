
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
	val restriction = License.SITE "Limited time trial license."
	val serialNumber = getIntVal "serialNumber"
	val maxMajorVersion = 9999
	val maxMinorVersion = 9999
	val date =
	    let
		val dateVal = 30 * secondsPerDay
	    in
		if dateVal = 0 then NONE
		else SOME (Date.fromTimeLocal (Time.+(Time.fromSeconds(IntInf.fromInt dateVal), Time.now())))
	    end
	val version = License.TRIAL
				 
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
