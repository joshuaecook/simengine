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

fun inst2props f : FunProps.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=f) classes)
	 of SOME (c as {name,properties,inputs,outputs,iterators,exps}) => {name=Symbol.name f,
									    operands=FunProps.FIXED (length (!inputs)),
									    precedence=1,
									    commutative=false,
									    associative=false,
									    eval=FunProps.INSTANCE,
									    text=(Symbol.name f, FunProps.PREFIX),
									    C=(Symbol.name f, FunProps.PREFIX),
									    expcost=0, (* need to work on this ... *)
									    codomain=fn(_) => [1]} (*TODO: ??? *)
	  | NONE => (Logger.log_internalerror (Printer.$("Can't handle operation '" ^ (Symbol.name f) ^ "'. Doesn't exist in current classes: {"
							 ^(String.concatWith ", " (map (fn{name,...}=>Symbol.name name) classes))^ "}"));
		     DynException.setErrored();
		     {name=Symbol.name f,
		      operands=FunProps.FIXED 0,
		      precedence=instancePrecedence,
		      commutative=false,
		      associative=false,
		      eval=FunProps.INSTANCE,
		      text=("<?" ^ (Symbol.name f) ^ ">", FunProps.PREFIX),
		      C=("<?" ^ (Symbol.name f) ^ ">", FunProps.PREFIX),
		      expcost=0,
		      codomain=(fn(_) => [1])})
    end
    handle e => DynException.checkpoint "Inst.inst2props" e


end
