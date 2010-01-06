


structure SimexStructs = struct
fun bug y = raise Fail y

type simengine = DL.library

(* Must export heap allocation functions
 * for all types of arrays returned
 * from external libraries. *)
val _ = _export "heap_alloc_word8": (int * Word8.word -> Word8.word array) -> unit;
    Array.array
val _ = _export "heap_update_word8": (Word8.word array * int * Word8.word -> unit) -> unit;
    Array.update

val _ = _export "heap_alloc_word32": (int * Word32.word -> Word32.word array) -> unit;
    Array.array
val _ = _export "heap_update_word32": (Word32.word array * int * Word32.word -> unit) -> unit;
    Array.update

val _ = _export "heap_alloc_word64": (int * Word64.word -> Word64.word array) -> unit;
    Array.array
val _ = _export "heap_update_word64": (Word64.word array * int * Word64.word -> unit) -> unit;
    Array.update

val _ = _export "heap_alloc_pointer": (int * MLton.Pointer.t -> MLton.Pointer.t array) -> unit;
    Array.array
val _ = _export "heap_update_pointer": (MLton.Pointer.t array * int * MLton.Pointer.t -> unit) -> unit;
    Array.update



structure API = struct
type api = DL.symbol
type meta = MLton.Pointer.t

fun get lib = DL.symbol (lib, "seint")

val metadata = _import "seint_metadata": api -> meta;
val version = _import "seint_version": api -> int;
val iteratorNames = _import "seint_iterator_names": api -> Char.char vector vector;
val defaultStates = _import "seint_default_states": api -> Real64.real vector;
val inputNames = _import "seint_input_names": api -> Char.char vector vector;
val defaultInputs = _import "seint_default_inputs": api -> Real64.real vector;
val outputNames = _import "seint_output_names": api -> Char.char vector vector;
val outputNumQuantities = _import "seint_output_num_quantities": api -> Int32.int vector;
val name = _import "seint_name": api -> Char.char vector;

structure Metadata = struct
val hashcode = _import "semeta_hashcode": meta -> Int64.int;
val numModels = _import "semeta_num_models": meta -> Int32.int;
val solverNames = _import "semeta_solver_names": meta -> Char.char vector vector;
val target = _import "semeta_target": meta -> Char.char vector;
datatype prec = Double | Single
local val precision' = _import "semeta_precision": meta -> Int32.int;
in fun precision meta = 
       case precision' meta
	of 4 => Single | 8 => Double
	 | _ => bug "odd precision"
end
end (* Metadata *)
end (* API *)

structure Result = struct
type result = unit
type status = unit

fun outputs (results, modelid) = bug "stub"
fun finalStates (results, modelid) = bug "stub"
fun finalTime (results, modelid) = bug "stub"
fun release results = bug "stub"
end (* Result *)

fun new filename = 
    DL.new (filename, DL.RTLD_NOW)

fun release sim = 
    DL.release sim

fun api sim = API.get sim

end (* SimexStructs *)


structure Simex: SIMEX = struct
open SimexStructs

datatype runparam = Run of {startTime: int,
			    stopTime: int,
			    numModels: int,
			    inputs: Real64.real vector,
			    states: Real64.real vector}

fun withSimengine filename f =
    let val simengine = new filename
    in f simengine before release simengine
    end

fun run (engine, Run {startTime, stopTime, numModels, inputs, states}) =
    bug "stub"
end (* Simex *)
