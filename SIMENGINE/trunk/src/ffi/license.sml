signature LICENSE =
sig
    (* license info data types*)
    datatype product = SIMENGINE
    datatype version = TRIAL | BASIC | STANDARD | PROFESSIONAL | DEVELOPMENT

    datatype restriction = USERNAME of string
			 | HOSTID of string
			 | LICENSESERVER of string
			 | SITE of string

    type enhancements = {}

    type license

    (* default license *)
    val default : license

    (* constructor *)
    val make: {product: product,
	       customerID: int,
	       customerName: string,
	       customerOrganization: string,
	       restriction: restriction,
	       serialNumber: int,
	       maxMajorVersion: int,
	       maxMinorVersion: int,
	       expirationDate: Date.date option,
	       version: version,
	       enhancements: enhancements} -> license

    (* accessor methods *)
    val product: license -> product
    val customerID: license -> int
    val customerName: license -> string
    val customerOrganization: license -> string
    val restriction: license -> restriction
    val serialNumber: license -> int
    val maxMinorVersion: license -> int
    val maxMajorVersion: license -> int
    val expirationDate: license -> Date.date option
    val version: license -> version
    val enhancements: license -> enhancements


    (* low level routines to convert Licenses to/from internal SML structure and external string format *)
    val licenseFromData : string -> license
    val licenseToData : license -> string

end

structure License: LICENSE =
struct

exception InvalidLicenseFile

(* the various forms of base packages we offer *)
datatype product = SIMENGINE
datatype version = TRIAL | BASIC | STANDARD | PROFESSIONAL | DEVELOPMENT

datatype restriction = USERNAME of string
		     | HOSTID of string
		     | LICENSESERVER of string
		     | SITE of string

type enhancements = {}

datatype license = LICENSE of
	 {product: product,
	  customerID: int,
	  customerName: string,
	  customerOrganization: string,
	  restriction: restriction,
	  serialNumber: int,
	  maxMajorVersion: int,
	  maxMinorVersion: int,
	  expirationDate: Date.date option,
	  version: version,
	  enhancements: enhancements}

val make = LICENSE

local fun acc f (LICENSE x) = f x
in
val product = acc #product
val customerID = acc #customerID
val customerName = acc #customerName
val customerOrganization = acc #customerOrganization
val restriction = acc #restriction
val serialNumber = acc #serialNumber
val maxMinorVersion = acc #maxMinorVersion
val maxMajorVersion = acc #maxMajorVersion
val expirationDate = acc #expirationDate
val version = acc #version
val enhancements = acc #enhancements
end

val basic_license = 
    make {product=SIMENGINE,
	     customerID=0,
	     customerName="Free User",
	     customerOrganization="Personal",
	     restriction=SITE "Anywhere",
	     serialNumber=0,
	     maxMajorVersion=1,
	     maxMinorVersion=1,
	     expirationDate=NONE,
	     version=BASIC,
	     enhancements={}}

val default = basic_license



val currentLicenseHeader = 0x00cafe00


val licenseDecode' = 
    _import "license_Decode": (Int32.int * string) -> Int32.int;

val licenseEncode' =
    _import "license_Encode": (Int32.int * string) -> Int32.int;

fun licenseDecode cipher =
    case licenseDecode' (String.size cipher, cipher)
     of 0 => FFIExports.getTheString () 
      | n => raise Fail ("Failed to decode license; error " ^ (Int.toString n))

fun licenseEncode licenseData =
    case licenseEncode' (String.size licenseData, licenseData)
     of 0 => FFIExports.getTheString () 
      | n => raise Fail ("Failed to encode license; error " ^ (Int.toString n))

fun bytesToInt bytes =
    LargeWord.toInt (PackWord32Big.subVec (bytes, 0))

fun intToBytes int =
    let val bytes = Word8Array.array (4, 0w0)
    in PackWord32Big.update (bytes, 0, Word64.fromInt int)
     ; Word8Array.vector bytes
    end

fun deqInt str =
    (String.extract (str, 4, NONE),
     bytesToInt (Byte.stringToBytes (String.substring (str, 0, 4))))

fun enqInt (str, int) =
    String.concat [str, Byte.bytesToString (intToBytes int)]

fun deqStr (str, len) = (String.extract(str, len, NONE), String.extract(str, 0, SOME len))

fun enqStr (str, s) = String.concat [str, s]






(* Decomposes a packed string license format into the SML license type *)
fun licenseFromData data = 
    let
	val licenseData = licenseDecode data
	val (licenseData, licenseHeader) = deqInt(licenseData)
	val _ = if (licenseHeader <> currentLicenseHeader) then
		    raise InvalidLicenseFile
		else
		    ()
	val (licenseData, productCode) = deqInt(licenseData)
	val product : product = case productCode of
				    0 => SIMENGINE
				  | _ => raise InvalidLicenseFile
	val (licenseData, serialNumber) = deqInt(licenseData)
	val (licenseData, productVersionCode) = deqInt(licenseData)
	val productVersion : version = case productVersionCode of
					   0 => BASIC
					 | 1 => STANDARD
					 | 2 => PROFESSIONAL
					 | 3 => DEVELOPMENT
					 | 4 => TRIAL
					 | _ => raise InvalidLicenseFile
	val (licenseData, majorVersion) = deqInt(licenseData)
	val (licenseData, minorVersion) = deqInt(licenseData)
	val (licenseData, customerID) = deqInt(licenseData)
	val (licenseData, restrictionCode) = deqInt(licenseData)
	val (licenseData, expiration) = deqInt(licenseData)
	val expirationDate = case expiration of
				 0 => NONE
			       | _ => SOME (Date.fromTimeLocal(Time.fromSeconds(IntInf.fromInt(expiration))))
	val (licenseData, enhancements) = deqInt(licenseData)
	val (licenseData, customerNameLen) = deqInt(licenseData)
	val (licenseData, customerOrgLen) = deqInt(licenseData)
	val (licenseData, restrictionLen) = deqInt(licenseData)
	val (licenseData, customerName) = deqStr(licenseData, customerNameLen)
	val (licenseData, customerOrg) = deqStr(licenseData, customerOrgLen)
	val (licenseData, restriction) = deqStr(licenseData, restrictionLen)
	val restriction = case restrictionCode of
			      0 => USERNAME restriction
			    | 1 => HOSTID restriction
			    | 2 => LICENSESERVER restriction
			    | 3 => SITE restriction
			    | _ => raise InvalidLicenseFile
	val lic =
	    LICENSE {product=product,
		     customerID=customerID,
		     customerName=customerName,
		     customerOrganization=customerOrg,
		     restriction=restriction,
		     serialNumber=serialNumber,
		     maxMajorVersion=majorVersion,
		     maxMinorVersion=minorVersion,
		     expirationDate=expirationDate,
		     version=productVersion,
		     enhancements={}}
    in
	lic
    end

fun licenseToData (LICENSE
		       {product, customerID, customerName, customerOrganization, restriction, serialNumber,
			maxMajorVersion, maxMinorVersion, expirationDate, version, enhancements}) = 
    let
	val licenseData = ""
	val licenseData = enqInt(licenseData, currentLicenseHeader)
	val product = case product of
			  SIMENGINE => 0
	val licenseData = enqInt(licenseData, product)
	val licenseData = enqInt(licenseData, serialNumber)
	val productVersion = case version of
				 BASIC => 0
			       | STANDARD => 1
			       | PROFESSIONAL => 2
			       | DEVELOPMENT => 3
			       | TRIAL => 4
	val licenseData = enqInt(licenseData, productVersion)
	val licenseData = enqInt(licenseData, maxMajorVersion)
	val licenseData = enqInt(licenseData, maxMinorVersion)
	val licenseData = enqInt(licenseData, customerID)
	val (restrictionCode, restriction) = case restriction of
						 USERNAME s => (0, s)
					       | HOSTID s => (1, s)
					       | LICENSESERVER s => (2, s)
					       | SITE s => (3, s)
	val licenseData = enqInt(licenseData, restrictionCode)
	val expiration = case expirationDate of
			     NONE => 0
			   | SOME dt => IntInf.toInt(Time.toSeconds (Date.toTime(dt)))
	val licenseData = enqInt(licenseData, expiration)
	val enhancements = 0
	val licenseData = enqInt(licenseData, enhancements)
	val licenseData = enqInt(licenseData, String.size(customerName))
	val licenseData = enqInt(licenseData, String.size(customerOrganization))
	val licenseData = enqInt(licenseData, String.size(restriction))
	val licenseData = enqStr(licenseData, customerName)
	val licenseData = enqStr(licenseData, customerOrganization)
	val licenseData = enqStr(licenseData, restriction)
    in
	licenseEncode licenseData
    end

end
