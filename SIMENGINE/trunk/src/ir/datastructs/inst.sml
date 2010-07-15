structure Inst =
struct

fun inst2classform f =
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=f) classes)
	 of SOME {properties={classform,...},...} => classform
	  | NONE => DynException.stdException(("No such class with name '"^(Symbol.name f)^"' found"), "Inst.inst2classform", Logger.INTERNAL)
    end
    handle e => DynException.checkpoint "Inst.inst2classform" e

val instancePrecedence = 1

fun generate_props {name, num_inputs} : FunProps.op_props = 
    let
	val classes = CurrentModel.classes()
	val funname = Symbol.name name
    in
	{name=funname,
	 operands=FunProps.FIXED (num_inputs),
	 precedence=1,
	 commutative=false,
	 associative=false,
	 eval=FunProps.INSTANCE,
	 text=(funname, FunProps.PREFIX),
	 C=(funname, FunProps.PREFIX),
	 mathematica=(funname, FunProps.PREFIX),
	 expcost=0, (* need to work on this ... *)
	 codomain=fn(_) => [1]} (*TODO: ??? *)
    end
    handle e => DynException.checkpoint "Inst.inst2props" e

fun inst2props f : FunProps.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=f) classes)
	 of SOME (c as {name,properties,inputs,outputs,exps}) => 
	    generate_props {name=f, num_inputs=length (!inputs)}
	  | NONE => (Logger.log_error (Printer.$("Can't handle operation '" ^ (Symbol.name f) ^ "'. Doesn't exist in current classes: {"
						 ^(String.concatWith ", " (map (fn{name,...}=>Symbol.name name) classes))^ "}"));
		     DynException.setErrored();
		     generate_props {name=f, num_inputs=0})
    end
    handle e => DynException.checkpoint "Inst.inst2props" e



end
