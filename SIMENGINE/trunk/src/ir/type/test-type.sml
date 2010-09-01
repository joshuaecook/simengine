functor TestType(T: TYPE) = struct

local 
    val pass = ()
    fun fail m = raise Fail m

    open T
    (* Create a few basic types. *)
    val int32 = int 32
    val int64 = int 64
    val real64 = real 64

    datatype hasatype 
      = HAS of (T.context,T.kind) T.typet
      | ANOTHER of hasatype * hasatype

in
val toString = toString
val app = apply

val gen = gen

val int = int32
val real = real64
val tuple = tuple

val intpair = tuple(int, int)
val realpair = tuple(real, real)
val intreal = tuple(int, real)
val realint = tuple(real, int)

val alist_to_a = poly(fn a => arrow(app(array,var a),var a))
val intlist_to_int = app(alist_to_a,int32)

val atuple = poly(fn a => 
		     tuple (var a, int))

val intarray = app(array, int)

val intpairarray = app(array, intpair)

val apair_list = poly(fn a =>
			 app(array,tuple(var a,var a)))

(* val b_list = poly(fn b => *)
(* 		     tuple(var b,  *)
(* 			   poly(fn a => *)
(* 				   app(array,tuple(var a,var a))))) *)

val b_list = poly(fn b =>
		     poly (fn a => 
			      tuple(var b, app(array,tuple(var a, var a)))))

val thing
  = [ANOTHER (HAS (gen(int)), HAS (gen(app(array, int)))),
     HAS (gen(app(atuple, int))),
     HAS (gen(alist_to_a)),
     HAS (gen(intlist_to_int)),
     HAS (gen(b_list))
    ]


val _ = 
    case equiv (intpair, intpair)
     of false => raise Fail "equiv (intpair, intpair)"
      | _ => pass
val _ =
    case equiv (array, array)
     of false => raise Fail "equiv (array, array)"
      | _ => pass
(* This is a static error: a proper type cannot be equivalent to an type operator. 
val _ =
    case equiv (intpair, array)
     of _ => raise Fail "bug"
*)
(* Generalized types can be compared. *)
val _ =
    case equiv (gen intpair, gen array)
     of true => raise Fail "equiv (gen intpair, gen array)"
      | _ => pass
val _ =
    case equiv (intpair, intpairarray)
     of true => raise Fail "equiv (intpair, intpairarray)"
      | _ => pass
val _ =
    case equiv (b_list,b_list)
     of false => raise Fail "equiv (b_list,b_list)"
      | _ => pass


val _ =
    case normal (intlist_to_int)
     of Arrow (Apply (Array, Primitive p1), Primitive p2) => 
	if p1 <> p2
	then raise Fail "normal (intlist_to_int)"
	else pass 
      |  _ => raise Fail "normal (intlist_to_int)"


end

end

structure Test = TestType(Type)
