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
		    toJSON: 'a -> JSON.json}


val realCalculus : real calculus = 
    {zero=0.0,
     isZero= (fn(a)=> Real.== (a, 0.0)),
     one=1.0,
     isOne= (fn(a)=> Real.== (a, 1.0)),
     addition= foldl (op +) 0.0,
     multiplication= foldl (op * ) 1.0,
     toString= Util.r2s,
     toJSON= JSON.real}

val intCalculus : int calculus = 
    {zero=0,
     isZero= (fn(a)=> a = 0),
     one=1,
     isOne= (fn(a)=> a = 1),
     addition= foldl (op +) 0,
     multiplication= foldl (op * ) 1,
     toString= Util.i2s,
     toJSON= JSON.int o IntInf.fromInt}

end
