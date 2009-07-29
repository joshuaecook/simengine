structure ComparisonLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments
and reals2boolfun = LibraryUtil.reals2boolfun

fun error msg =
    Logger.log_usererror [PosLog.NOPOS] (Printer.$ msg)

fun gt _ = reals2boolfun Real.>
fun ge _ = reals2boolfun Real.>=
fun lt _ = reals2boolfun Real.<
fun le _ = reals2boolfun Real.<=

fun eq _ args =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r1), KEC.LITERAL(KEC.CONSTREAL r2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (Real.?=(r1, r2) andalso ((Real.isNan r1) = (Real.isNan r2))))
      |	[KEC.LITERAL(KEC.CONSTSTR s1), KEC.LITERAL(KEC.CONSTSTR s2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (s1 = s2)) 
      |	[KEC.LITERAL(KEC.CONSTBINARY (size1, val1)), KEC.LITERAL(KEC.CONSTBINARY (size2, val2))] =>
	KEC.LITERAL(KEC.CONSTBOOL (size1 = size2 andalso val1 = val2))
     |	[KEC.LITERAL(KEC.CONSTBOOL b1), KEC.LITERAL(KEC.CONSTBOOL b2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (b1 = b2))
      | [KEC.OBJECT {members=members1, ...}, KEC.OBJECT {members=members2, ...}]
	=> KEC.LITERAL(KEC.CONSTBOOL(members1 = members2))
      | [arg1, arg2]
	=> KEC.LITERAL(KEC.CONSTBOOL(false))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

fun ne _ args =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r1), KEC.LITERAL(KEC.CONSTREAL r2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (not (Real.?=(r1, r2))))
      |	[KEC.LITERAL(KEC.CONSTSTR s1), KEC.LITERAL(KEC.CONSTSTR s2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (s1 <> s2))
      |	[KEC.LITERAL(KEC.CONSTBINARY (size1, val1)), KEC.LITERAL(KEC.CONSTBINARY (size2, val2))] =>
	KEC.LITERAL(KEC.CONSTBOOL (size1 <> size2 orelse val1 <> val2))
      |	[KEC.LITERAL(KEC.CONSTBOOL b1), KEC.LITERAL(KEC.CONSTBOOL b2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (b1 <> b2))
      | [KEC.OBJECT {members=members1, ...}, KEC.OBJECT {members=members2, ...}]
	=> KEC.LITERAL(KEC.CONSTBOOL(members1 <> members2))
      | [arg1, arg2]
	=> KEC.LITERAL(KEC.CONSTBOOL(true))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}
	
fun isdefined _ args =
    case args of
	[KEC.UNDEFINED] => KEC.LITERAL (KEC.CONSTBOOL false)
      | [_] => KEC.LITERAL (KEC.CONSTBOOL true)
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="gt", operation=gt},
	       {name="ge", operation=ge},
	       {name="lt", operation=lt},
	       {name="le", operation=le},
	       {name="eq", operation=eq},
	       {name="neq", operation=ne},
	       {name="isdefined", operation=isdefined}
	      ]

end
