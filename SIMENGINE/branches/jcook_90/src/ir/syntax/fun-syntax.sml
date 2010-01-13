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
		     string (#name (FunProps.op2props operation)))
  | toJSON (Fun.INST {classname, instname, props}) =
    JSONTypedObject ("FUN.INST",
		     object [("classname", symbol classname),
			     ("instname", symbol instname),
			     ("properties", propertiesToJSON props)])

and propertiesToJSON {inline, iterators, realclassname, realinstname, sourcepos} =
    object [("inline", bool inline),
	    ("iterators", array (map symbol iterators)),
	    ("realClassName", JSONOption (symbol, realclassname)),
	    ("realInstanceName", JSONOption (symbol, realinstname)),
	    ("sourcePosition", JSONOption (PosLog.toJSON, sourcepos))]

end
