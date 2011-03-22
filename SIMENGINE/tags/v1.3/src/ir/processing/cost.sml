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


signature COST=
sig

    (* Compute the costs based on the op props in fun_properties.sml *)
    val exp2cost : Exp.exp -> int
    val class2cost : DOF.class -> int
    val model2cost : DOF.model -> int
    val model2uniquecost : DOF.model -> int

    (* display information on model costs - useful for profiling *)
    val logModelCosts : DOF.model -> unit

end
structure Cost : COST =
struct

fun getclass classname =
    SOME (CurrentModel.classname2class classname)
    handle DynException.NoClassFound _ => NONE
    
fun exp2generalcost deep exp =
    let 
	val exp2cost = exp2generalcost deep
	val class2cost = class2generalcost deep
    in
	case exp of
	    Exp.TERM t => 0
	  | Exp.FUN (Fun.BUILTIN f, args) => #expcost (MathFunctionProperties.op2props f) +
					     Util.sum (map exp2cost args)
	  | Exp.FUN (Fun.INST {classname, instname, props}, args) => 
	    let
		val args_size = Util.sum (map exp2cost args)
		val inst_size = case getclass classname of
				    SOME c =>
				    if deep then
					class2cost c
				    else
					0
				  | NONE => ~1
	    in
		args_size + inst_size
	    end
	  | Exp.FUN (Fun.OUTPUT _, args) => (* TODO *) 1
	  | Exp.META (Exp.SEQUENCE s) => Util.sum (map exp2cost s)
	  | Exp.META _ => 0
	  | Exp.CONTAINER c => Util.sum (map exp2cost (Container.containerToElements c))
    end
    
and class2generalcost deep (c:DOF.class) = 
    let
	val exp2cost = exp2generalcost deep
	val inputs = !(#inputs c)
	val exps = !(#exps c)
	val outputs = !(#outputs c)
    in
	Util.sum ((map exp2cost (List.mapPartial DOF.Input.default inputs)) @
		  (map exp2cost (map DOF.Output.condition outputs @ 
				 (Util.flatmap DOF.Output.contents outputs))) @
		  (map exp2cost exps))
    end

and model2generalcost deep ((classes,{name,classname},_):DOF.model) =
    if deep then
	case List.find (fn{name,...}=>name=classname) classes of
	    SOME c => class2generalcost deep c
	  | NONE => 0
    else
	Util.sum (map (class2generalcost deep) classes)

val exp2cost = exp2generalcost true
val class2cost = class2generalcost true
val model2cost = model2generalcost true

val model2uniquecost = model2generalcost false

fun logModelCosts model = 
    let
	val total_cost = model2cost model
	val unique_cost = model2uniquecost model
    in
	Util.log("Expression Total Cost: "^ (Util.i2s total_cost) ^ "; Unique Cost: " ^ (Util.i2s unique_cost))
    end



end
