structure FunProcess = 
struct

fun fun2name f = case f of
		      Fun.BUILTIN sym => sym
		    | Fun.INST {classname=sym,...} => sym		     

fun fun2props f = case f of
		      Fun.BUILTIN sym => Fun.builtin2props sym
		    | Fun.INST {classname=sym,...} => Inst.inst2props sym

end
