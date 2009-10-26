structure DOF =
struct 

type expression = Exp.exp

type inputproperties =
     {defaultValue: Exp.term option,
      sourcepos: PosLog.pos}
      
datatype classtype = MASTER of Symbol.symbol
		   | SLAVE of Symbol.symbol

datatype classform = FUNCTIONAL
		   | INSTANTIATION of {readstates: Symbol.symbol list,
				       writestates: Symbol.symbol list}


type classproperties = {sourcepos: PosLog.pos,
			basename: Symbol.symbol,
			classform: classform,
			classtype: classtype}

datatype iteratortype = CONTINUOUS of Solver.solver
		      | DISCRETE of {sample_period:real} (* sampling frequency *)
		      | POSTPROCESS of Symbol.symbol
		      | UPDATE of Symbol.symbol

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


type class = {name:Symbol.symbol,
	      properties:classproperties,
	      (*		  inputs: (Symbol.symbol * inputproperties) list ref,*)
	      inputs: {name: Exp.term, default: expression option} list ref,
	      outputs: {name: Exp.term, 
			contents: expression list, 
			condition: expression} list ref,
	      iterators: classiterator list,
	      exps: expression list ref}
	     
type instance = {name: Symbol.symbol option,
		 classname: Symbol.symbol}
		    
(* The instance identifies the outermost class. *)
type model = class list * instance * systemproperties

	
end
