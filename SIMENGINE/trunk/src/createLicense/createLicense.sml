(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)


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
