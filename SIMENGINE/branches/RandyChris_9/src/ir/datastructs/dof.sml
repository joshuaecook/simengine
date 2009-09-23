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


type classproperties = {sourcepos: PosLog.pos, classform: classform, classtype: classtype}

datatype iteratortype = CONTINUOUS of Solver.solver
		      | DISCRETE of {fs:real} (* sampling frequency *)

datatype precisiontype = SINGLE | DOUBLE

type systemproperties = {iterators: (Symbol.symbol * iteratortype) list, 
			 time: (real * real),
			 precision: precisiontype}

type class = {name:Symbol.symbol,
	      properties:classproperties,
	      (*		  inputs: (Symbol.symbol * inputproperties) list ref,*)
	      inputs: {name: Exp.term, default: expression option} list ref,
	      outputs: {name: Exp.term, 
			contents: expression list, 
			condition: expression} list ref,
	      iterators: {name: Symbol.symbol,
			  low: real,
			  step: real,
			  high: real} list,
	      exps: expression list ref}
	     
type instance = {name: Symbol.symbol option,
		 classname: Symbol.symbol}
		    
type model = class list * instance * systemproperties

	
end
