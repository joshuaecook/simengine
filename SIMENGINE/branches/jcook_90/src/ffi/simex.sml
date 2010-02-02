


structure SimexStructs = struct
fun bug y = raise Fail y

type simengine = DL.library

structure API = struct
type api = MLton.Pointer.t

fun get lib = 
    let val getinterface = DL.function (lib, "simengine_getinterface")
	val wrapper = _import * : DL.function -> unit -> api;
    in 
	wrapper getinterface ()
    end


val name = _import "seint_name": api -> Char.char vector;
val target = _import "seint_target": api -> Char.char vector;
val solverNames = _import "seint_solver_names": api -> Char.char vector vector;
val iteratorNames = _import "seint_iterator_names": api -> Char.char vector vector;
val inputNames = _import "seint_input_names": api -> Char.char vector vector;
val stateNames = _import "seint_state_names": api -> Char.char vector vector;
val outputNames = _import "seint_output_names": api -> Char.char vector vector;
val defaultInputs = _import "seint_default_inputs": api -> Real64.real vector;
val defaultStates = _import "seint_default_states": api -> Real64.real vector;
val outputNumQuantities = _import "seint_output_num_quantities": api -> Int32.int vector;
val version = _import "seint_version": api -> int;
datatype prec = Double | Single
local val precision' = _import "seint_precision": api -> Int32.int;
in fun precision api = 
       case precision' api
	of 4 => Single | 8 => Double
	 | _ => bug "odd precision"
end
val numModels = _import "seint_num_models": api -> Int32.int;
val numIterators = _import "seint_num_iterators": api -> Int32.int;
val numInputs = _import "seint_num_inputs": api -> Int32.int;
val numStates = _import "seint_num_states": api -> Int32.int;
val numOutputs = _import "seint_num_outputs": api -> Int32.int;
val hashcode = _import "seint_hashcode": api -> Int64.int;
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
