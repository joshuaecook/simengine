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

		       
local
    structure T = Type
    open Sail
    structure Op = Operator
    structure A = Atom
    structure L = Abstraction
    structure Exp = Expression
    structure B = Binding


    local
	open T
	(* Create a few basic types. *)
	val int32 = int 32
	val int64 = int 64
	val real64 = real 64
    in
    val a_t = (*poly (fn a => apply (array,a))*)
	apply(array, int32)
    val aa_t = gen(tuple (a_t, a_t))
    val a_t = gen a_t

    end

    val a_var = fn x => TypeApplication.TypeApply {var= Atom.Variable x, args= Vector.fromList [a_t]}

    val aa_var = fn x => TypeApplication.TypeApply {var= Atom.Variable x, args= Vector.fromList [aa_t]}


(*		  
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
*)

    local
	open SailBuild
    in
    val simple_list_fun = 
	fn oper =>
	   let val body
		 = Exp.new {bindings= Vector.fromList [B.Value {var= ("y", a_t), 
								object= oper (sailvar ("x", int32))}], 
			    result= sailvar ("y", int32)}
	   in
	       L.Lambda {param= Vector.fromList [("x", a_t)],
			 body= body}
	   end
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

(*val _ = SailUtil.print_task mergesort*)
val _ = SailUtil.print_sml_task mergesort

end
