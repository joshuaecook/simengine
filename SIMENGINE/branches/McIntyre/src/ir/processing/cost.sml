signature COST=
sig

    (* Compute the costs based on the op props in fun_properties.sml *)
    val exp2cost : Exp.exp -> int
    val class2cost : DOF.class -> int
    val model2cost : DOF.model -> int

end
structure Cost : COST =
struct

fun exp2cost exp =
    case exp of
	Exp.TERM t => 0
      | Exp.FUN (Fun.BUILTIN f, args) => #expcost (FunProps.op2props f) +
					 Util.sum (map exp2cost args)
      | Exp.FUN (Fun.INST {classname, instname, props}, args) => 
	let
	    val args_size = Util.sum (map exp2cost args)
	    val c = CurrentModel.classname2class classname
	    val inst_size = class2cost c
	in
	    args_size + inst_size
	end
      | Exp.META (Exp.SEQUENCE s) => Util.sum (map exp2cost s)
      | Exp.META _ => 0
      | Exp.CONTAINER c => container2cost c

and container2cost container =
    case container of
	Exp.EXPLIST l => Util.sum (map exp2cost l)
      | Exp.VECTOR v => Vector.foldl (fn(e,cost)=>cost + (exp2cost e)) 0 v
      | Exp.MATRIX m => Array2.fold Array2.RowMajor (fn(e, cost)=>cost+(exp2cost e)) 0 m

and class2cost (c:DOF.class) = 
    let
	val inputs = !(#inputs c)
	val exps = !(#exps c)
	val outputs = !(#outputs c)
    in
	Util.sum ((map exp2cost (List.mapPartial #default inputs)) @
		  (map exp2cost (map #condition outputs @ (Util.flatmap #contents outputs))) @
		  (map exp2cost exps))
    end

and model2cost ((classes,{name,classname},_):DOF.model) =
    case List.find (fn{name,...}=>name=classname) classes of
	SOME c => class2cost c
      | NONE => 0

end
