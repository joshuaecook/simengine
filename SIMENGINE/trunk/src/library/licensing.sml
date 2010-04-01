structure LicensingLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun licenseProductType exec args = 
    case args
     of nil => 
	KEC.LITERAL (KEC.CONSTSTR (CurrentLicense.versionToString ()))
      | args => raise IncorrectNumberOfArguments {expected=0, actual=(length args)}

val library = [{name="licenseProductType", operation=licenseProductType}]

end
