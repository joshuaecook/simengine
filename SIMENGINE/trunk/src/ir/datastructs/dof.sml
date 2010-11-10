structure DOF =
struct 

type expression = Exp.exp
 and term = Exp.term

structure Input: sig
    type input
    datatype behaviour =
	     HOLD | HALT | CYCLE

    val name: input -> term
    val default: input -> expression option
    val behaviour: input -> behaviour

    (* Returns an identical input with a new name. *)
    val rename: (term -> term) -> input -> input
    (* Returns an input with a new default value rewritten by the given function. 
     * The function is only invoked for default=(SOME x) and cannot be used to rewrite
     * default=NONE to default=(SOME y).
     *)
    val rewrite: (expression -> expression) -> input -> input

    val make: {name: term, default: expression option, behaviour: behaviour} -> input
end = struct
datatype input = 
	 INPUT of {name: term,
		   default: expression option,
		   behaviour: behaviour}
     and behaviour =
	 HOLD | HALT | CYCLE

val make = INPUT

local fun attr f (INPUT x) = f x
in
val name = attr #name
val default = attr #default
val behaviour = attr #behaviour
end

fun rename f input =
    INPUT {name=f (name input), default=default input, behaviour=behaviour input}

fun rewrite f input =
    INPUT {default=Option.map f (default input), name=name input, behaviour=behaviour input}
end (* structure Input *)


structure Output: sig
    type output

    val name: output -> term
    val inputs: output -> term list ref
    val contents: output -> expression list
    val condition: output -> expression

    (* Returns an identical output with a new name. *)
    val rename: (term -> term) -> output -> output
    val renameInputs: (term -> term) -> output -> output
    (* Returns an output with expression with new contents and conditions
     * rewritten by the given function. *)
    val rewrite: (expression -> expression) -> output -> output

    val make: {name: term, inputs: term list ref, contents: expression list, condition: expression} -> output
end = struct
datatype output = 
	 OUTPUT of {name: term,
		    inputs: term list ref,
		    contents: expression list,
		    condition: expression}

val make = OUTPUT
local fun attr f (OUTPUT x) = f x
in
val name = attr #name
val inputs = attr #inputs
val contents = attr #contents
val condition = attr #condition
end

fun rename f output =
    OUTPUT {name=f (name output), inputs=inputs output, contents=contents output, condition=condition output}

fun renameInputs f output =
    OUTPUT {name=name output, inputs=ref (map f (!(inputs output))), contents=contents output, condition=condition output}

fun rewrite f output =
    OUTPUT {name=name output, inputs=inputs output, contents=map f (contents output), condition=f (condition output)}
end (* structure Output *)

structure Class: sig
(*
    type class
    datatype form 
      = INSTANTIATION of {readstates: Symbol.symbol list,
			  writestates: Symbol.symbol list}
    datatype properties 
      = {sourcepos: PosLog.pos,
	 preshardname: Symbol.symbol,
	 classform: form}

    val name: class -> Symbol.symbol
    val properties: class -> properties
    val inputs: class -> Input.input list
    val outputs: class -> Output.output list
    val expressions: class -> Exp.exp list

    val setInputs: class * Input.input list -> unit
    val setOutputs: class * Output.output list -> unit
    val setExpressions: class * Exp.exp list -> unit
*)
end = struct
end (* structure Class *)



datatype classform 
  = INSTANTIATION of {readstates: Symbol.symbol list,
		      writestates: Symbol.symbol list}


type classproperties = {sourcepos: PosLog.pos,
			preshardname: Symbol.symbol,
			classform: classform}

datatype precisiontype = SINGLE | DOUBLE

open Iterator (* pull in all the iterator definitions *)
type systemiterator = iteratordef

type systemproperties = {iterators: systemiterator list, 
			 precision: precisiontype,
			 target: Target.target,
			 parallel_models: int,
			 debug: bool,
			 profile: bool}


type class = {name: Symbol.symbol,
	      properties: classproperties,
	      inputs: Input.input list ref,
	      outputs: Output.output list ref,
	      exps: expression list ref}
	     
type instance = {name: Symbol.symbol option,
		 classname: Symbol.symbol}
		    
(* The instance identifies the outermost class. *)
type model = class list * instance * systemproperties




end
