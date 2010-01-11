(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

signature JSON_PRINT_STRUCTS = sig
    structure JS: JSON
end

signature JSON_PRINT = sig
    include JSON_PRINT_STRUCTS
    val print: TextIO.outstream * JS.json -> unit
    val printFile: string * JS.json -> unit
end

functor PrintJSON (S: JSON_PRINT_STRUCTS): JSON_PRINT = struct
open S

fun toJSONString json =
    case JS.value json
     of JS.JS_NULL => "null"
      | JS.JS_TRUE => "true"
      | JS.JS_FALSE => "false"
      | JS.JS_OBJECT => objectToJSONString json
      | JS.JS_ARRAY => arrayToJSONString json
      | JS.JS_STRING => stringToJSONString json
      | JS.JS_NUMBER =>
	if isSome (JS.toInt json) then intToJSONString json else
	if isSome (JS.toReal json) then realToJSONString json else
	raise Fail ("Unknown type of JSON value")

and stringToJSONString json = 
    String.concat ["\"", String.toCString (valOf (JS.toString json)), "\""]
and realToJSONString json =
    let val r = JS.realVal json
    in
	if Real.isFinite r then
	    (if 0.0 > r then "-" ^ (Real.toString (~r)) else 
	     Real.toString r) else
	if Real.isNan r then stringToJSONString (JS.string "NaN") else
	if 0.0 > r then stringToJSONString (JS.string "-Infinity") else
	stringToJSONString (JS.string "Infinity")
    end
and intToJSONString json =
    let val z = JS.intVal json
    in
	if 0 > z then "-" ^ (IntInf.toString (~z)) else IntInf.toString z
    end
and boolToJSONString json =
    if JS.boolVal json then "true" else "false"
and arrayToJSONString json =
    let val elements = valOf (JS.elements json)
    in
	String.concat ["[", String.concatWith "," (map toJSONString elements), "]"]
    end
and objectToJSONString json =
    let val members = ListPair.unzip (valOf (JS.members json))
	fun pairToString (name, value) =
	    String.concat [stringToJSONString (JS.string name), ":", toJSONString value]
    in
	String.concat ["{", String.concatWith "," (ListPair.map pairToString members), "}"]
    end

fun print (outstream, json) =
    TextIO.output (outstream, toJSONString json)

fun printFile (filename, json) =
    let val outstream = TextIO.openOut filename
    in
	print (outstream, json) before TextIO.closeOut outstream
    end
end
