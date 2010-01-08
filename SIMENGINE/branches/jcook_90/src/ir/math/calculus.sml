(* Calculus Structure - provides a means of describing how to do calculate generically across multiple data types *)
(* There are calculi described here for integer and real math.  There is an Exp.exp calculus in exp.sml.  There can be a calculus
 * constructed for model operations in the front end.  These are right now used exclusively for matrices, but can be added to 
 * arrays and other data structures *)
structure Calculus =
struct

type 'a calculus = {zero: 'a,
		    isZero: 'a -> bool,
		    one: 'a,
		    isOne: 'a -> bool,
		    addition: 'a list -> 'a,
		    multiplication: 'a list -> 'a,
		    toString: 'a -> string,
		    toJSON: 'a -> mlJS.json_value}


val realCalculus : real calculus = 
    {zero=0.0,
     isZero= (fn(a)=> Real.== (a, 0.0)),
     one=1.0,
     isOne= (fn(a)=> Real.== (a, 1.0)),
     addition= foldl (op +) 0.0,
     multiplication= foldl (op * ) 1.0,
     toString= Util.r2s,
     toJSON= mlJS.js_float}

val intCalculus : int calculus = 
    {zero=0,
     isZero= (fn(a)=> a = 0),
     one=1,
     isOne= (fn(a)=> a = 1),
     addition= foldl (op +) 0,
     multiplication= foldl (op * ) 1,
     toString= Util.i2s,
     toJSON= mlJS.js_int}

end
