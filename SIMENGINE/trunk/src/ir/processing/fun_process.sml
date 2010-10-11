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

fun equal (Fun.BUILTIN sym1, Fun.BUILTIN sym2) = sym1 = sym2
  | equal (Fun.INST {classname=sym1, ...}, Fun.INST {classname=sym2,...}) = sym1 = sym2
  | equal (Fun.OUTPUT {classname=sym1, outname=name1, ...}, Fun.OUTPUT {classname=sym2, outname=name2,...}) = sym1 = sym2 andalso name1 = name2
  | equal _ = false


end
