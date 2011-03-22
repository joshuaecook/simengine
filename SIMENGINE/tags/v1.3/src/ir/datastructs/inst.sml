(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

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

fun generate_props {name, num_inputs} : MathFunctionProperties.op_props = 
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
	 codomain=fn(_) => [1]} (*TODO: ??? *)
    end
    handle e => DynException.checkpoint "Inst.inst2props" e

fun inst2props f : MathFunctionProperties.op_props = 
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

fun output2props (classname, outname) : MathFunctionProperties.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=classname) classes)
	 of SOME (c as {name,properties,inputs,outputs,exps}) => 
	    generate_props {name=outname, num_inputs=length (!inputs)}
	  | NONE => (Logger.log_error (Printer.$("Can't handle operation '" ^ (Symbol.name classname) ^ "'. Doesn't exist in current classes: {"
						 ^(String.concatWith ", " (map (fn{name,...}=>Symbol.name name) classes))^ "}"));
		     DynException.setErrored();
		     generate_props {name=outname, num_inputs=0})
    end
    handle e => DynException.checkpoint "Inst.output2props" e



end
