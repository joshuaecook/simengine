signature SIMENGINE_INTERFACE = sig
    structure Metadata : sig
	type t
	datatype precision = FLOAT | DOUBLE
	val hashcode: t -> Int64.int
	val parallelModels: t -> Int32.int
	val solvers: t -> string vector
	val target: t -> string
	val precision: t -> precision
    end

    type t
    val get: unit -> t
    val version: t -> Int32.int
    val iterators: t -> string vector
    val inputs: t -> string vector
    val states: t -> string vector
    val outputs: t -> string vector
    val outputNumQuantities: t -> Int32.int vector
    val defaultInputs: t -> Real64.real vector
    val defaultStates: t -> Real64.real vector
    val name: t -> string
    val metadata: t -> Metadata.t
end

structure SimEngineInterface:> SIMENGINE_INTERFACE = struct

type t = MLton.Pointer.t

val _ = _export "alloc_char": (int -> char array) -> unit;
    (fn n => Array.array (n, #"\000"))
val _ = _export "alloc_int32": (int -> Int32.int array) -> unit;
    (fn n => Array.array (n, 0))
val _ = _export "alloc_real64": (int -> Real64.real array) -> unit;
    (fn n => Array.array (n, 0.0))
val _ = _export "alloc_string": (int -> string array) -> unit;
    (fn n => Array.array (n, ""))
val _ = _export "update_string": (string array * int * string -> unit) -> unit;
    Array.update

val get = _import "simengine_interface": unit -> t;
val version = _import "seint_version": t -> Int32.int;
val iterators = _import "seint_iterator_names": t -> string vector;
val inputs = _import "seint_input_names": t -> string vector;
val states = _import "seint_state_names": t -> string vector;
val outputs = _import "seint_output_names": t -> string vector;
val outputNumQuantities = _import "seint_output_num_quantities": t -> Int32.int vector;
val defaultInputs = _import "seint_default_inputs": t -> Real64.real vector;
val defaultStates = _import "seint_default_states": t -> Real64.real vector;
val name = _import "seint_name": t -> string;

structure Metadata = struct
type t = MLton.Pointer.t
datatype precision = FLOAT | DOUBLE
val hashcode = _import "semeta_hashcode": t -> Int64.int;
val parallelModels = _import "semeta_parallel_models": t -> Int32.int;
val solvers = _import "semeta_solvers": t -> string vector;
val target = _import "semeta_target": t -> string;
val precision' = _import "semeta_precision": t -> Int32.int;
fun precision meta =
    case precision' meta
     of 4 => FLOAT
      | 8 => DOUBLE
      | i => raise Fail ("Unsupported precision " ^ (Int32.toString i))
end

val metadata = _import "seint_metadata": t -> Metadata.t;

end



local open SimEngineInterface
in 
val seint = get ()
val _ = print ("simEngine version " ^ (Int32.toString (version seint)) ^ "\n")
val inputs = defaultInputs seint
val _ = print ("simEngine has " ^ (Int.toString (Vector.length inputs)) ^ " default inputs:")
val _ = Vector.app (fn x => print (" " ^ (Real64.toString x))) inputs
val _ = print ("\n")

val _ = print ("simEngine name " ^ (name seint) ^ "\n")

val _ = print ("simEngine has " ^ (Int.toString (Vector.length (iterators seint))) ^ " iterators:")
val _ = Vector.app (fn x => print (" " ^ x)) (iterators seint)
val _ = print ("\n")

val meta = metadata seint
val _ = print ("target name " ^ (Metadata.target meta) ^ "\n")
val _ = case Metadata.precision meta
	 of Metadata.FLOAT => print "single precision\n"
	  | Metadata.DOUBLE => print "double precision\n"
end
