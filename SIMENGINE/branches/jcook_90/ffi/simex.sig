(* The public API of a compiled simEngine simulation library. *)
signature SIMENGINE_API = sig
    type api

    val name: api -> string
    val target: api -> string
    val solverNames: api -> string vector
    val iteratorNames: api -> string vector
    val inputNames: api -> string vector
    val stateNames: api -> string vector
    val outputNames: api -> string vector
    val defaultInputs: api -> Real64.real vector
    val defaultStates: api -> Real64.real vector
    val outputNumQuantities: api -> Int32.int vector
    val version: api -> int
    datatype prec = Double | Single
    val precision: api -> prec
    val numModels: api -> int
    val numIterators : api -> int
    val numInputs : api -> int
    val numStates : api -> int
    val numOutputs : api -> int
    val hashcode: api -> Int64.int

end

(* Results returned from a simulation. *)
signature SIMENGINE_RESULT_STRUCTS = sig
    type result
    type status
end

signature SIMENGINE_RESULT = sig
    include SIMENGINE_RESULT_STRUCTS

    (* Returns outputs for a given model id. *)
    val outputs: result * int -> Real64.real vector
    (* Returns final states for a given model id. *)
    val finalStates: result * int -> Real64.real vector
    (* Returns final time for a given model id. *)
    val finalTime: result * int -> Real64.real vector
			    
    val release: result -> unit
end

(* A general-purpose user interface for a simulation. *)
signature SIMEX_STRUCTS = sig
    structure API: SIMENGINE_API
    structure Result: SIMENGINE_RESULT

    type simengine

    val new: string -> simengine
    val release: simengine -> unit
    val api: simengine -> API.api
end

signature SIMEX = sig
    include SIMEX_STRUCTS

    datatype runparam = Run of {startTime: int,
				stopTime: int,
				numModels: int,
				inputs: Real64.real vector,
				states: Real64.real vector}

    val withSimengine: string -> (simengine -> 'a) -> 'a
    val run: simengine * runparam -> Result.result
end
