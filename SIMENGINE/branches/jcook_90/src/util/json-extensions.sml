(* Copyright (C) 2010 by Simatra Modeline Technologies, L.L.C. *)

structure JSONExtensions = struct
open JSON
(* Extensions to JSON used extensively by internal serializers. *)

datatype data = JSON_TYPE of string
datatype constructor = JSON_CONSTRUCTOR of string * json

fun JSONTypedObject (typ, value) =
    object [("$type", string typ),
	    ("$value", value)]

fun fromJSONTypedObject json =
    case memberValue (json, "$type", toString)
     of SOME s => 
	SOME (JSON_CONSTRUCTOR (s, valOf (member (json, "$value"))))
      | NONE => NONE

	handle Option => NONE

fun JSONType (typ) = 
    object [("$type", string typ)]

fun fromJSONType (json) =
    case memberValue (json, "$type", toString)
     of SOME s => SOME (JSON_TYPE s)
      | NONE => NONE

fun JSONOption (toJSON, SOME x) = toJSON x
  | JSONOption _ = JSON.null

end
