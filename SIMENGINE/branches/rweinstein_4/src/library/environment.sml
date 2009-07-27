structure EnvironmentLib =
struct

val getEnv = OS.Process.getEnv
val setEnv = MLton.ProcEnv.setenv

val TypeMismatch = DynException.TypeMismatch
and ValueError = DynException.ValueError
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun env_getVar _ args = 
    case args of
	[KEC.LITERAL (KEC.CONSTSTR name)] =>
	(case getEnv name of
	     SOME var => KEC.LITERAL (KEC.CONSTSTR var)
	   | NONE => KEC.UNDEFINED)
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


fun env_setVar _ args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR name), KEC.LITERAL (KEC.CONSTSTR value)] =>
	KEC.UNIT before
	setEnv {name=name, value=value}
      | [a, b] => raise TypeMismatch ("expected 2 strings but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}
		      
val library = [{name="getEnv", operation=env_getVar},
	       {name="setEnv", operation=env_setVar}]
	      
end
