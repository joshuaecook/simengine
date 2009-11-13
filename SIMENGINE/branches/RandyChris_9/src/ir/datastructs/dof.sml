structure DOF =
struct 

type expression = Exp.exp

type inputproperties =
     {defaultValue: Exp.term option,
      sourcepos: PosLog.pos}
  
(* The master/slave relationship between classes enforces ordering. *)    
datatype classtype
  = MASTER of Symbol.symbol
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
			classform: classform,
			classtype: classtype}

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
  | POSTPROCESS of Symbol.symbol
  (* An immediate iterator is used for outputs having no other iterator. *)
  | IMMEDIATE

datatype precisiontype = SINGLE | DOUBLE

type systemiterator = (Symbol.symbol * iteratortype)

type systemproperties = {iterators: systemiterator list, 
			 precision: precisiontype,
			 target: Target.target,
			 num_models: int,
			 debug: bool,
			 profile: bool}

type classiterator = {name: Symbol.symbol,
		      low: real,
		      step: real,
		      high: real}


type input = {name: Exp.term, 
	      default: expression option}
type output = {name: Exp.term, 
	       contents: expression list, 
	       condition: expression}

type class = {name: Symbol.symbol,
	      properties: classproperties,
	      inputs: input list ref,
	      outputs: output list ref,
	      iterators: classiterator list,
	      exps: expression list ref}
	     
type instance = {name: Symbol.symbol option,
		 classname: Symbol.symbol}
		    
(* The instance identifies the outermost class. *)
type model = class list * instance * systemproperties

	
end
