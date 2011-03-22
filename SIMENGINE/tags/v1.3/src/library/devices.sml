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
