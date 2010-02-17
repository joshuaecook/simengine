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

				       
val library = [{name="timestampSeconds", operation=sys_timestampSeconds},
	       {name="timestampString", operation=sys_timestampString}]
end
