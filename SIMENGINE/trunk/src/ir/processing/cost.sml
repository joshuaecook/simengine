
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
	  | _ => Util.sum (map exp2cost (ExpTraverse.level exp))

(*
	  | Exp.FUN (Fun.OUTPUT _, args) => (* TODO *) 1
	  | Exp.META (Exp.SEQUENCE s) => Util.sum (map exp2cost s)
	  | Exp.META _ => 0
	  | Exp.CONTAINER c => Util.sum (map exp2cost (Container.containerToElements c))
	  | Exp.CONVERSION (Exp.SUBREF (exp', subspace)) => exp2generalcost deep exp' (* TODO - fix this and only look at the subspace *)
	  | Exp.CONVERSION (Exp.RESHAPE (exp', space)) => exp2generalcost deep exp'
*)
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
