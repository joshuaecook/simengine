structure Devices = struct
  (* Add gpu device_props to ffi? *)

  val openmpGetNumProcessors = 
      _import "omp_get_num_procs": (unit) -> int;
end
