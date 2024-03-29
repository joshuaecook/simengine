structure FunProcess = 
struct

fun fun2name f = case f of
		      Fun.BUILTIN sym => Symbol.symbol (#name (FunProps.op2props sym))
		    | Fun.INST {classname=sym,...} => sym		     

fun fun2props f = case f of
		      Fun.BUILTIN sym => FunProps.op2props sym
		    | Fun.INST {classname=sym,...} => Inst.inst2props sym

end
