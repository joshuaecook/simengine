(* The public API of a compiled simEngine simulation library. *)
signature SIMENGINE_API_STRUCTS = sig
    type api
    type meta

    val version: api -> int
    val metadata: api -> meta
end

signature SIMENGINE_API = sig
    include SIMENGINE_API_STRUCTS

    structure Metadata : sig
	val hashcode: meta -> Int64.int
	val numModels: meta -> int
	val solverNames: meta -> string vector
	val target: meta -> string
	datatype prec = Double | Single
	val precision: meta -> prec
    end

    val iteratorNames: api -> string vector
    val defaultStates: api -> Real64.real vector
    val inputNames: api -> string vector
    val defaultInputs: api -> Real64.real vector
    val outputNames: api -> string vector
    val outputNumQuantities: api -> Int32.int vector

    val name: api -> string
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

    val run: simengine * runparam -> Result.result
end
