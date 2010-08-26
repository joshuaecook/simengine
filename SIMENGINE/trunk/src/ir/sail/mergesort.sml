		       
local
    open Sail
    structure Op = Operator
    structure A = Atom
    structure L = Abstraction
    structure Exp = Expression
    structure B = Binding

    val a_t = () (*Type.VAR "a"*)
    val a_var = fn x => TypeApplication.TypeApply {var= x, args= Vector.fromList [a_t]}

    val aa_t = () (*Type.PAIR (a_t, a_t)*)
    val aa_var = fn x => TypeApplication.TypeApply {var= x, args= Vector.fromList [aa_t]}

    val simple_list_fun 
      = fn oper =>
	   let val body
		 = Exp.new {bindings= Vector.fromList [B.Value {var= ("y", a_t), 
								object= oper (a_var "x")}], 
			    result= a_var "y"}
	   in
	       L.Lambda {param= Vector.fromList [("x", a_t)],
			 body= body}
	   end

in
val mergesort =
    let
	val divisible = simple_list_fun Op.Array.null
	val divide = simple_list_fun Op.Array.split
	val sort = simple_list_fun Op.Array.sort

	val merge
	  = let val body
		  = Exp.new {bindings= Vector.fromList [B.Value {var= ("x", a_t), object= Op.Pair.one (aa_var "xy")},
							B.Value {var= ("y", a_t), object= Op.Pair.two (aa_var "xy")},
							B.Value {var= ("z", a_t), object= Op.Array.concat (a_var "x", a_var "y")}],
			     result= a_var "z"}
	    in
		L.Lambda {param= Vector.fromList [("xy", a_t)],
			  body= body}
	    end
    in
	L.DivideAndConquer {divisible=divisible, 
			    divide=divide,
			    task=sort,
			    merge=merge}
    end

val _ = SailUtil.print_task mergesort

end
