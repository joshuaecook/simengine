structure FunProcess = 
struct

fun fun2name f = case f of
		      Fun.BUILTIN v => Symbol.symbol (MathFunctionProperties.op2name v)
		    | Fun.INST {classname=sym,...} => sym
		    | Fun.OUTPUT {outname,...} => outname
		     

fun fun2props f = case f of
		      Fun.BUILTIN sym => MathFunctionProperties.op2props sym
		    | Fun.INST {classname=sym,...} => Inst.inst2props sym
		    | Fun.OUTPUT {classname,outname,...} => 
		      Inst.output2props (classname, outname)

end
