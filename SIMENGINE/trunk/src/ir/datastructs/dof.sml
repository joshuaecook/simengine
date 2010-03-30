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
    val contents: output -> expression list
    val condition: output -> expression

    (* Returns an identical output with a new name. *)
    val rename: (term -> term) -> output -> output
    (* Returns an output with expression with new contents and conditions
     * rewritten by the given function. *)
    val rewrite: (expression -> expression) -> output -> output

    val make: {name: term, contents: expression list, condition: expression} -> output
end = struct
datatype output = 
	 OUTPUT of {name: term,
		    contents: expression list,
		    condition: expression}

val make = OUTPUT
local fun attr f (OUTPUT x) = f x
in
val name = attr #name
val contents = attr #contents
val condition = attr #condition
end

fun rename f output =
    OUTPUT {name=f (name output), contents=contents output, condition=condition output}

fun rewrite f output =
    OUTPUT {name=name output, contents=map f (contents output), condition=f (condition output)}
end (* structure Output *)

(* The master/slave relationship between classes enforces ordering. *)    
datatype classtype
  = MASTER (*of Symbol.symbol*)
  | SLAVE of Symbol.symbol


(* A class may take the form of a model instance or a functional
 * invocation. Functional forms are not currently used. What are
 * they for? *)
datatype classform 
  = INSTANTIATION of {readstates: Symbol.symbol list,
		      writestates: Symbol.symbol list}
  | FUNCTIONAL


type classproperties = {sourcepos: PosLog.pos,
			basename: Symbol.symbol,
			preshardname: Symbol.symbol,
			classform: classform,
			classtype: classtype}

(* All algebraic iterators have a process type which describes when in the execution loop a particular equation will be evaluated *)
datatype processtype = (* assuming iterator t*)
	 PREPROCESS (* x[t] = f(x[t]) *)
       | INPROCESS (* x[t+1] = f(x[t]) *)
       | POSTPROCESS (* x[t+1] = f(x[t+1]) *)

datatype iteratortype 
  (* A continuous iterator, e.g. t, is in the real domain and
   * has an associated numerical solver. *)
  = CONTINUOUS of Solver.solver
  (* A discrete iterator, e.g. n, is self-evaluating and
   * has a period relative to the global time scale. *)
  | DISCRETE of {sample_period:real}
  (* An update iterator is dependent upon another named iterator. 
   * Update evaluations occur after the primary evalation. *)
  | UPDATE of Symbol.symbol
  (* A postprocess iterator is dependent upon another named iterator. 
   * Postprocess evaluations occur after primary evaluation and 
   * any update evaluations. *)
  | ALGEBRAIC of (processtype * Symbol.symbol)
  (* An immediate iterator is used for outputs having no other iterator. *)
  | IMMEDIATE

datatype precisiontype = SINGLE | DOUBLE

type systemiterator = (Symbol.symbol * iteratortype)

type systemproperties = {iterators: systemiterator list, 
			 precision: precisiontype,
			 target: Target.target,
			 parallel_models: int,
			 debug: bool,
			 profile: bool}

type classiterator = {name: Symbol.symbol,
		      low: real,
		      step: real,
		      high: real}

datatype inputMeans =
	 CONSTANT
       | SAMPLE of {iterator: Symbol.symbol,
		    behaviour: sampleInputBehaviour}
     (* TODO
       | STREAM
       | EVENT
      *)
     and sampleInputBehaviour =
	 STOP | REPEAT | HOLD of expression


type class = {name: Symbol.symbol,
	      properties: classproperties,
	      inputs: Input.input list ref,
	      outputs: Output.output list ref,
	      iterators: classiterator list,
	      exps: expression list ref}
	     
type instance = {name: Symbol.symbol option,
		 classname: Symbol.symbol}
		    
(* The instance identifies the outermost class. *)
type model = class list * instance * systemproperties




end
