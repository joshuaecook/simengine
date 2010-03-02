structure DevicesLib = struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun openmpGetNumProcessors exec =
 fn [] 
    => KEC.LITERAL(KEC.CONSTREAL(Real.fromInt(Devices.openmpGetNumProcessors())))
  | args => raise IncorrectNumberOfArguments {expected = 0, actual = length args}

fun cudaDeviceProps exec =
 fn [] =>
    let val props = Devices.cudaDeviceProps ()
    in
	exec (KEC.list2kecvector (map (KEC.LITERAL o KEC.CONSTSTR) props))
    end
  | args => raise IncorrectNumberOfArguments {expected = 0, actual = length args}

fun cudaDevicePropsError exec =
 fn [] =>
    KEC.LITERAL (KEC.CONSTSTR (Devices.cudaDevicePropsError ()))
  | args => raise IncorrectNumberOfArguments {expected = 0, actual = length args}


val library = [{name = "openmpGetNumProcessors", operation = openmpGetNumProcessors},
	       {name = "cudaDeviceProps", operation = cudaDeviceProps},
	       {name = "cudaDevicePropsError", operation = cudaDevicePropsError}]


end
