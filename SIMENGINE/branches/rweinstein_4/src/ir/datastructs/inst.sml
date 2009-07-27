structure Inst =
struct

fun inst2props f : Fun.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	(case (List.find (fn({name,...}:DOF.class)=>name=f) classes)
	  of SOME {name,properties,inputs,outputs,eqs} => {name=Symbol.name f,
							   operands=Fun.FIXED (length (!inputs)),
							   precedence=1,
							   commutative=false,
							   associative=false,
							   text=(Symbol.name f, Fun.PREFIX),
							   C=(Symbol.name f, Fun.PREFIX)}
	   | NONE => (print ("Can't handle operation '" ^ (Symbol.name f) ^ "'\n");
		      DynException.stdException(("No class with name '"^(Symbol.name f)^"' defined"), "Inst.fun2props", Logger.INTERNAL)))
    end

(* handle instance properties *)
val emptyinstprops = {dim=NONE,
		      sourcepos=NONE,
		      realname=NONE,
		      form=NONE}

fun setRealName (props as {dim, sourcepos, realname, form}) sym = 
    {dim=dim,
    sourcepos=sourcepos,
    realname=SOME sym,
    form=form}
															 
end
