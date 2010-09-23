structure TimeLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun sys_timestampSeconds _ _ =
    KEC.LITERAL (KEC.CONSTREAL (Real.fromLargeInt(Time.toSeconds(Time.now()))))
    handle e => DynException.checkpoint "SystemLib.timestampSeconds" e

fun sys_timestampString _ _ =
    KEC.LITERAL(KEC.CONSTSTR (Date.toString(Date.fromTimeLocal(Time.now()))))
    handle e => DynException.checkpoint "TimeLib.timestampString" e

fun sys_daysToString exec args = 
    case args 
     of [KEC.LITERAL (KEC.CONSTREAL r)] => 
	KEC.LITERAL(KEC.CONSTSTR (Util.daysToString(Date.fromTimeUniv(Time.fromReal(r*3600.0*24.0)))))
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
	
				       
val library = [{name="timestampSeconds", operation=sys_timestampSeconds},
	       {name="timestampString", operation=sys_timestampString},
	       {name="daysToString", operation=sys_daysToString}]
end
