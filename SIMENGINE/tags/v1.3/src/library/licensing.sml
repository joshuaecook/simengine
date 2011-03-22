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

exception InvalidLicense
val InvalidLicenseError = (KEC.ERROR o KEC.LITERAL o KEC.CONSTSTR) "Invalid License"
fun licenseToJSON (str_list as first::rest) =
    let
	val prev_license = CurrentLicense.get()
	val license_str = String.concatWith "\n" str_list
	val lic = case L.licenseFromData license_str of
		      SOME license => license
		    | NONE => raise InvalidLicense
	val _ = CurrentLicense.set(lic)
	val result = (licenseToJSON [])
		     handle e => (CurrentLicense.set(prev_license);
				  raise e)
	val _ = CurrentLicense.set(prev_license)
    in
	result
    end
  | licenseToJSON nil =
    (PrintJSON.toString (License.toJSON (CurrentLicense.get ())))

fun licenseToJSON' l = 
    (KEC.LITERAL o KEC.CONSTSTR) (licenseToJSON l)
    handle InvalidLicense => InvalidLicenseError
    

val library = [{name="licenseProductType", 
		operation = fn exec => unitToStringFun CurrentLicense.versionToString},
	       {name="licenseToJSON", 
		operation = fn exec => strListToKEC licenseToJSON'},
	       {name="defaultTarget",
		operation = fn exec => unitToStringFun Features.defaultTarget},
	       {name="licenseCustomerName", 
		operation = fn exec => unitToStringFun CurrentLicense.customerName},
	       {name="licenseCustomerOrganization", 
		operation = fn exec => unitToStringFun CurrentLicense.customerOrganization},
	       {name="licenseExpirationDate", operation=licenseExpirationDate},
	       {name="daysTillExpiration", operation=daysTillExpiration}]

end
