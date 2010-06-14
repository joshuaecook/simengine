structure PrecisionLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

exception Aborted

fun std_quantify _ (args: KEC.exp list) =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r)] => KEC.LITERAL(KEC.CONSTREAL (RangeBits.real2quantreal r))
      | [exp] => raise TypeMismatch ("expected a number but received " ^ (PrettyPrint.kecexp2nickname exp))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_num2bits exec (args: KEC.exp list) =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r)] => 
	let
	    val (_, {sign, bits, frac}) = RangeBits.num2bits r (RangeBits.accuracy())

	in
	    exec (KEC.APPLY{func=KEC.SEND {message=Symbol.symbol "new",
					   object=KEC.SYMBOL (Symbol.symbol "FixptPrecision")}, 
			    args=KEC.TUPLE [KEC.LITERAL(KEC.CONSTREAL (Real.fromInt sign)),
					    KEC.LITERAL(KEC.CONSTREAL (Real.fromInt bits)),
					    KEC.LITERAL(KEC.CONSTREAL (Real.fromInt frac))]})
	end		 
      | [exp] => raise TypeMismatch ("expected a number but received " ^ (PrettyPrint.kecexp2nickname exp))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="quantify", operation=std_quantify},
	       {name="num2fixpt", operation=std_num2bits}]

end
