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
      = HAS of T.t
      | ANOTHER of hasatype * hasatype

fun tuple (s1,s2) = record (("1",s1), ("2",s2))

in
val app = apply

val gen = gen

val int = int32
val real = real64

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

(* This is a static error; polymorphic types are not first-class values.
val b_list = poly(fn b =>
		     tuple(var b,
			   poly(fn a =>
				   app(array,tuple(var a,var a)))))
*)

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
    case (rep o normal) intlist_to_int
     of Arrow (Apply (Array, Int b1), Int b2)
	=> (case Size.Bits.compare (b1,b2)
	     of EQUAL => pass
	      | _ => raise Fail "normal (intlist_to_int)")
      |  _ => raise Fail "normal (intlist_to_int)"



val _ =
    case subtype (bool, int32)
     of false => raise Fail "subtype (bool, int32)"
      | _ => pass

val _ =
    case subtype (bool, intarray)
     of false => raise Fail "subtype (bool, intarray)"
      | _ => pass


end

end

structure Test = TestType(Type)
