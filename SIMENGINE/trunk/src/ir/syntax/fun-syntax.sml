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

(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure FunSyntax: sig
(* Serialization and deserialization for DOF function data. *)

val toJSON: Fun.funtype -> JSON.json
(* TODO implement fromJSON *)

end = struct

open JSON
open JSONExtensions

fun symbol s = JSON.object [("$symbol", JSON.string (Symbol.name s))]


fun toJSON (Fun.BUILTIN operation) =
    JSONTypedObject ("FUN.BUILTIN",
		     string (MathFunctionProperties.op2name operation))
  | toJSON (Fun.INST {classname, instname, props}) =
    JSONTypedObject ("FUN.INST",
		     object [("classname", symbol classname),
			     ("instname", symbol instname),
			     ("properties", propertiesToJSON props)])
  | toJSON (Fun.OUTPUT {classname, instname, outname, props}) =
    JSONTypedObject ("FUN.INST",
		     object [("classname", symbol classname),
			     ("instname", symbol instname),
			     ("outname", symbol outname),
			     ("properties", propertiesToJSON props)])

and propertiesToJSON {inline, iterators, realclassname, sourcepos} =
    object [("inline", bool inline),
	    ("iterators", array (map symbol iterators)),
	    ("realClassName", JSONOption (symbol, realclassname)),
	    ("sourcePosition", JSONOption (PosLog.toJSON, sourcepos))]

end
