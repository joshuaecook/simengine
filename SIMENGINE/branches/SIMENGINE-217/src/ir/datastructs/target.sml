(* Data structure for target specific information *)
structure Target =
struct

(* TODO encode more metadata in this datatype. *)
datatype target 
  = CPU (* generic C target *)
  | OPENMP (* using the underlying OpenMP libraries *)
  | CUDA (*of {arch: cudaArchitecture,
	     deviceId: int,
	     emulate: bool}*)

and cudaArchitecture 
  = COMPUTE_10
  | COMPUTE_11
  | COMPUTE_12
  | COMPUTE_13
  | COMPUTE_20


(* of {compute: computecapability, (* right now, this can be 1.1 or 1.3 *)
	     multiprocessors: int, (* number of multiprocessors *)
	     globalMemory: int (* amount of global memory *)} *)

(* for CUDA, there are two compute capabilities that are supported, 1.1 and 1.3 *)
(* and computecapability = COMPUTE11 | COMPUTE14 *)

(* debugging commands *)
val i2s = Util.i2s
fun target2str t =
    case t 
     of CPU => "CPU"
      | OPENMP => "OPENMP"
      | CUDA => "CUDA" (*{compute=COMPUTE11, multiprocessors, globalMemory} => "CUDA {compute capability: 1.1, # of multi-processors: "^(i2s multiprocessors)^", global memory (KB): "^(i2s globalMemory)^"}"
      | CUDA {compute=COMPUTE13, multiprocessors, globalMemory} => "CUDA {compute capability: 1.3, # of multi-processors: "^(i2s multiprocessors)^", global memory (KB): "^(i2s globalMemory)^"}" *)

end
