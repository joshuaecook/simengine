(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

(* The default structures for parsing and printing JSON data. *)
structure ParseJSON = ParseJSON(structure JS = JSON structure Token = JSONToken structure Lex = LexJSON)
structure PrintJSON = PrintJSON(structure JS = JSON)

(* Extensions to JSON used extensively by internal serializers. *)

fun JSONTypedObject (typ, value) =
    JSON.object [("$type", JSON.string typ),
		 ("$value", value)]

fun JSONType (typ) = 
    JSON.object [("$type", JSON.string typ)]

fun JSONOption (toJSON, SOME x) = toJSON x
  | JSONOption _ = JSON.null

