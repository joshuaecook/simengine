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

fun generate_props {name, num_inputs, space} : MathFunctionProperties.op_props = 
    let
	val classes = CurrentModel.classes()
	val funname = Symbol.name name
    in
	{name=funname,
	 operands=MathFunctionProperties.FIXED (num_inputs),
	 precedence=1,
	 commutative=false,
	 associative=false,
	 eval=MathFunctionProperties.INSTANCE,
	 text=(funname, MathFunctionProperties.PREFIX),
	 C=(funname, MathFunctionProperties.PREFIX),
	 mathematica=(funname, MathFunctionProperties.PREFIX),
	 expcost=0, (* need to work on this ... *)
	 codomain=fn(_) => space}
    end
    handle e => DynException.checkpoint "Inst.inst2props" e

fun error msg = (Logger.log_error (Printer.$ msg); DynException.setErrored())

fun inst2props (f, props) : MathFunctionProperties.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=f) classes)
	 of SOME (c as {name,properties,inputs,outputs,exps}) => 
	    generate_props {name=f, num_inputs=length (!inputs), space=InstProps.getSpace props}
	  | NONE => (error ("Can't handle operation '" ^ (Symbol.name f) ^ "'. Doesn't exist in current classes: {"
			    ^(String.concatWith ", " (map (fn{name,...}=>Symbol.name name) classes))^ "}");
		     generate_props {name=f, num_inputs=0, space=Space.emptyCollection})
    end
    handle e => DynException.checkpoint "Inst.inst2props" e

val expToSpace = ref (fn(exp)=>Space.emptyCollection)

fun output2props (classname, outname, props) : MathFunctionProperties.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=classname) classes)
	 of SOME (c as {name,properties,inputs,outputs,exps}) => 
	    let
		val output_space = case List.find (fn(out)=>Term.sym2symname (DOF.Output.name out) = outname) (!outputs) of
				       SOME out => (!expToSpace) (Exp.TERM (DOF.Output.name out))
				     | NONE => (error ("Output '"^(Symbol.name outname)^"' not found in model '"
						       ^(Symbol.name classname)^"'");
						Space.emptyCollection)
		val space = Space.multiply (output_space, InstProps.getSpace props)
	    in
		generate_props {name=outname, num_inputs=length (!inputs), space=space}
	    end
	  | NONE => (error ("Can't handle operation '" ^ (Symbol.name classname) ^ "'. Doesn't exist in current classes: {"
			    ^(String.concatWith ", " (map (fn{name,...}=>Symbol.name name) classes))^ "}");
		     generate_props {name=outname, num_inputs=0, space=Space.emptyCollection})
    end
    handle e => DynException.checkpoint "Inst.output2props" e



end
