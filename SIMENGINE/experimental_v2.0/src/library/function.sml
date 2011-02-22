structure FunctionLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

exception Aborted

fun std_apply (exec: KEC.exp -> KEC.exp) (args: KEC.exp list) =
    case args of
	func :: runargs :: nil => 
	let
	    val args = case runargs of KEC.UNIT => runargs
				     | KEC.TUPLE _ => runargs
				     | _ => KEC.TUPLE [runargs]
	in
	    exec (KEC.APPLY {func=func, args=args})
	end
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

val library = [{name="apply", operation=std_apply}]

end
