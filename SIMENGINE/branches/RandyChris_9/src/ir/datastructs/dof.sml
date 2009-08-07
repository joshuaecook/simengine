structure DOF =
struct 

type expression = Exp.exp

type inputproperties =
     {defaultValue: Exp.term option,
      sourcepos: PosLog.pos}
      
datatype classtype = MASTER of Symbol.symbol
		   | SLAVE of Symbol.symbol

datatype classform = FUNCTIONAL
		   | INSTANTIATION of {readstates:Symbol.symbol list,
				       writestates: Symbol.symbol list}


type classproperties = {sourcepos: PosLog.pos, classform: classform, classtype: classtype}

datatype iteratortype = CONTINUOUS of Solver.solver
		      | DISCRETE

datatype precisiontype = SINGLE | DOUBLE

type systemproperties = {iterators: (Symbol.symbol * iteratortype) list, 
			 time: (real * real),
			 precision: precisiontype}

datatype eq_type = INSTANCE of {name:Symbol.symbol, 
				classname: Symbol.symbol,
				offset: (Symbol.symbol * int) list}
		(* | FUNCTION of {classname: Symbol.symbol,
				offset: (Symbol.symbol * int) list}*)
		 | DIFFERENCE_EQ of {offset:int}
		 | DERIVATIVE_EQ of {offset:int}
		 | INTERMEDIATE_EQ 
		 | INITIAL_VALUE of {offset:int}


withtype class = {name:Symbol.symbol,
		  properties:classproperties,
(*		  inputs: (Symbol.symbol * inputproperties) list ref,*)
		  inputs: {name: Exp.term, default: expression option} list ref,
		  outputs: {name: Exp.term, 
			    contents: expression list, 
			    condition: expression} list ref,
		  iterators: {name: Symbol.symbol,
			      low: int,
			      high: int} list,
		  exps: expression list ref(*,
		  eqs: {eq_type: eq_type,
			sourcepos: PosLog.pos,
			lhs: Exp.term,
			rhs: expression} list ref*)}
			       
type eq = {eq_type: eq_type,
	   sourcepos: PosLog.pos,
	   lhs: Exp.term,
	   rhs: expression}

type instance = {name: Symbol.symbol option,
		 classname: Symbol.symbol}
		    
type model = class list * instance * systemproperties

	
end
