structure DevicesLib = struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun openmpGetNumProcessors exec =
 fn [] 
    => KEC.LITERAL(KEC.CONSTREAL(Real.fromInt(Devices.openmpGetNumProcessors())))
  | args => raise IncorrectNumberOfArguments {expected = 0, actual = length args}



val library = [{name = "openmpGetNumProcessors", operation = openmpGetNumProcessors}]


end
