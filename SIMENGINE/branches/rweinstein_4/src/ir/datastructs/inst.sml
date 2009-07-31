structure Inst =
struct

fun inst2props f : Fun.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=f) classes)
	 of SOME {name,properties,inputs,outputs,exps,eqs} => {name=Symbol.name f,
							       operands=Fun.FIXED (length (!inputs)),
							       precedence=1,
							       commutative=false,
							       associative=false,
							       text=(Symbol.name f, Fun.PREFIX),
							       C=(Symbol.name f, Fun.PREFIX)}
	  | NONE => (Logger.log_internalerror (Printer.$("Can't handle operation '" ^ (Symbol.name f) ^ ". Doesn't exist in current classes: "
							 ^(String.concatWith ", " (map (fn{name,...}=>Symbol.name name) classes))^ "}"));
		     DynException.setErrored();
		     {name=Symbol.name f,
		      operands=Fun.FIXED 0,
		      precedence=1,
		      commutative=false,
		      associative=false,
		      text=("<?" ^ (Symbol.name f) ^ ">", Fun.PREFIX),
		      C=("<?" ^ (Symbol.name f) ^ ">", Fun.PREFIX)})
    end
    handle e => DynException.checkpoint "Inst.inst2props" e


end
