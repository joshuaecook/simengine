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

structure Devices = struct
  val openmpGetNumProcessors = 
      _import "openmp_getNumProcessors": (unit) -> int;

  val cudaDeviceProps' =
      _import "cuda_getDeviceProps": (unit) -> int;

  val cudaDevicePropsError' =
      _import "cuda_devicePropsError": (unit) -> unit;


  (* FIXME recognize and report errors *)
  val error =
   fn 0 => NONE
    | n => 
      (cudaDevicePropsError' ()
     ; SOME (Fail (FFIExports.getTheString ())))

  fun cudaDeviceProps () =
      case error (cudaDeviceProps' ())
       of NONE =>
	  let 
	      fun loop k =
		  case FFIExports.popAString ()
		   of NONE => k nil
		    | SOME str => loop (fn strs => str :: (k strs))
	  in
	      loop (fn x => x)
	  end
	| SOME exn => nil

  fun cudaDevicePropsError () =
      (cudaDevicePropsError' ()
     ; FFIExports.getTheString ())
end
