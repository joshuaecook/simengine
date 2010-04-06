structure LicensingLib =
struct

open LibraryUtil
structure L = License

fun licenseExpirationDate exec args = 
    case args
     of nil => 
	let
	    val date = CurrentLicense.expirationDate()
	in
	    case date of 
		SOME date => KEC.LITERAL (KEC.CONSTSTR (Util.daysToString date))
	      | NONE => KEC.LITERAL (KEC.CONSTSTR "unlimited")
	end
      | args => raise IncorrectNumberOfArguments {expected=0, actual=(length args)}

fun daysTillExpiration exec args =
    case args
     of nil => 
	let
	    val current_date = Util.daysToday()
	    val date = case CurrentLicense.expirationDate() of
			   SOME date => Util.daysFromDate date
			 | NONE => (* return -1 *) (current_date- (IntInf.fromInt 1))
	in
	    KEC.LITERAL (KEC.CONSTREAL (Real.fromLargeInt (date - current_date)))
	end
      | args => raise IncorrectNumberOfArguments {expected=0, actual=(length args)}

fun licenseToJSON () =
    PrintJSON.toString (License.toJSON (CurrentLicense.get ()))

val library = [{name="licenseProductType", 
		operation = fn exec => unitToStringFun CurrentLicense.versionToString},
	       {name="licenseToJSON", 
		operation = fn exec => unitToStringFun licenseToJSON},
	       {name="licenseCustomerName", 
		operation = fn exec => unitToStringFun CurrentLicense.customerName},
	       {name="licenseCustomerOrganization", 
		operation = fn exec => unitToStringFun CurrentLicense.customerOrganization},
	       {name="licenseExpirationDate", operation=licenseExpirationDate},
	       {name="daysTillExpiration", operation=daysTillExpiration}]

end
