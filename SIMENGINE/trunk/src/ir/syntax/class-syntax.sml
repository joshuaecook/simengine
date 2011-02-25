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

structure ClassSyntax: sig
(* Serialization and deserialization for DOF class data. *)

val toJSON: DOF.class -> JSON.json
(* TODO implement fromJSON *)

end = struct

open JSON
open JSONExtensions

val int = int o IntInf.fromInt

fun symbol s = JSON.object [("$symbol", JSON.string (Symbol.name s))]

fun toJSON (class as {name, properties, inputs, outputs, exps}) =
    object [("name", symbol name),
	    ("properties", propertiesToJSON properties),
	    ("inputs", array (map inputToJSON (! inputs))),
	    ("outputs", array (map outputToJSON (! outputs))),
	    ("expressions", array (map ExpSyntax.toJSON (! exps)))]

and inputToJSON input =
    object [("name", ExpSyntax.termToJSON (DOF.Input.name input)),
	    ("default", JSONOption (ExpSyntax.toJSON, (DOF.Input.default input)))]

and outputToJSON output =
    object [("name", ExpSyntax.termToJSON (DOF.Output.name output)),
	    ("contents", array (map ExpSyntax.toJSON (DOF.Output.contents output))),
	    ("condition", ExpSyntax.toJSON (DOF.Output.condition output))]

and iteratorToJSON {name, low, step, high} =
    object [("name", symbol name),
	    ("low", real low),
	    ("step", real step),
	    ("high", real step)]

and propertiesToJSON {preshardname, classform, sourcepos} =
    object [("classForm", classFormToJSON classform),
	    ("preShardName", symbol preshardname),
	    ("sourcePosition", PosLog.toJSON sourcepos)]

and classFormToJSON (DOF.INSTANTIATION {readstates, writestates}) =
    JSONTypedObject ("DOF.INSTANTIATION",
		     object [("readStates", array (map symbol readstates)),
			     ("writeStates", array (map symbol writestates))])

end
