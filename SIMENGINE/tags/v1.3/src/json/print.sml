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

signature JSON_PRINT_STRUCTS = sig
    structure JS: JSON
end

signature JSON_PRINT = sig
    include JSON_PRINT_STRUCTS
    val toString: JS.json -> string
    val print: TextIO.outstream * JS.json -> unit
    val printFile: string * JS.json -> unit
end

functor PrintJSON (S: JSON_PRINT_STRUCTS): JSON_PRINT = struct
open S

fun toJSONString json =
    case JS.jsType json
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
    String.concat ["\"", String.toCString (JS.stringVal json), "\""]

and realToJSONString json =
    let val r = JS.realVal json
    in
	if Real.isFinite r then
	    String.translate (fn #"~" => "-" | c => str c) (Real.fmt StringCvt.EXACT r)
	else if Real.isNan r then stringToJSONString (JS.string "NaN") 
	else if 0.0 > r then stringToJSONString (JS.string "-Infinity") 
	else stringToJSONString (JS.string "Infinity")
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

val toString = toJSONString

fun print (outstream, json) =
    TextIO.output (outstream, toJSONString json)

fun printFile (filename, json) =
    let val outstream = TextIO.openOut filename
    in
	print (outstream, json) before TextIO.closeOut outstream
    end
end
