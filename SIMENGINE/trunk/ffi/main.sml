signature SIMENGINE_INTERFACE = sig
    type t
    val get : unit -> t
    val version : t -> Int32.int
    val numIterators : t -> Int32.int
    val numInputs : t -> Int32.int
    val numStates : t -> Int32.int
    val numOutputs : t -> Int32.int
    val defaultInputs : t -> Real64.real vector
    val name : t -> string
end

structure SimEngineInterface:> SIMENGINE_INTERFACE = struct

type t = MLton.Pointer.t

val _ = _export "alloc_int32": (int * Int32.int -> Int32.int array) -> unit;
    Array.array
val _ = _export "alloc_real64": (int * Real64.real -> Real64.real array) -> unit;
    Array.array

     
val get = _import "simengine_interface": unit -> t;
val version = _import "seint_version": t -> Int32.int;
val numIterators = _import "seint_num_iterators": t -> Int32.int;
val numInputs = _import "seint_num_inputs": t -> Int32.int;
val numStates = _import "seint_num_states": t -> Int32.int;
val numOutputs = _import "seint_num_outputs": t -> Int32.int;

val defaultInputs = _import "seint_default_inputs": t -> Real64.real vector;
val defaultStates = _import "seint_default_states": t -> Real64.real vector;

val name = _import "seint_name": t -> string;

end



val ffi = _import "ffi": Int32.int -> Int32.int;
val _ = print ("ffi(3) returns " ^ (Int32.toString (ffi(3))) ^ "\n")


local open SimEngineInterface
in 
val seint = get ()
val _ = print ("simEngine version " ^ (Int32.toString (version seint)) ^ "\n")
val _ = print ("simEngine numStates " ^ (Int32.toString (numStates seint)) ^ "\n")
val inputs = defaultInputs seint
val _ = print ("simEngine has " ^ (Int.toString (Vector.length inputs)) ^ " default inputs\n")
val _ = print ("simEngine first defaultInput " ^ (Real64.toString (Vector.sub (inputs,0))) ^ " default inputs\n")
end
