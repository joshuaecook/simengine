structure Devices = struct
  (* Add gpu device_props to ffi? *)

  val openmpGetNumProcessors = 
      _import "openmp_getNumProcessors": (unit) -> int;
end
