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
			classform: classform,
			classtype: classtype}

datatype iteratortype = CONTINUOUS of Solver.solver
		      | DISCRETE of {fs:real} (* sampling frequency *)
		      | POSTPROCESS of Symbol.symbol
		      | UPDATE of Symbol.symbol

datatype precisiontype = SINGLE | DOUBLE

type systemiterator = (Symbol.symbol * iteratortype)

type systemproperties = {iterators: systemiterator list, 
			 time: (real * real),
			 precision: precisiontype}

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
		    
type model = class list * instance * systemproperties

	
end
