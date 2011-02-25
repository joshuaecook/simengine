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
