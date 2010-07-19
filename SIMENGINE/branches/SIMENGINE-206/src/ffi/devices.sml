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
