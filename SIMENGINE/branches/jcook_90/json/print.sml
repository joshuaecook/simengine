signature PRINT_JSON = sig
    val print: TextIO.outstream * JSON.json -> unit
    val printFile: string * JSON.json -> unit
end

structure PrintJSON:> PRINT_JSON = struct
structure J = JSON

fun toJSONString json =
    case J.value json
     of J.JS_NULL => "null"
      | J.JS_TRUE => "true"
      | J.JS_FALSE => "false"
      | J.JS_OBJECT => objectToJSONString json
      | J.JS_ARRAY => arrayToJSONString json
      | J.JS_STRING => stringToJSONString json
      | J.JS_NUMBER =>
	if isSome (J.toInt json) then intToJSONString json else
	if isSome (J.toReal json) then realToJSONString json else
	raise Fail ("Unknown type of JSON value")

and stringToJSONString json = 
    String.concat ["\"", String.toCString (valOf (J.toString json)), "\""]
and realToJSONString json =
    let val r = valOf (J.toReal json)
    in
	if Real.isFinite r then
	    (if 0.0 > r then "-" ^ (Real.toString (~r)) else 
	     Real.toString r) else
	if Real.isNan r then stringToJSONString (J.string "NaN") else
	if 0.0 > r then stringToJSONString (J.string "-Infinity") else
	stringToJSONString (J.string "Infinity")
    end
and intToJSONString json =
    let val z = valOf (J.toInt json)
    in
	if 0 > z then "-" ^ (IntInf.toString (~z)) else IntInf.toString z
    end
and boolToJSONString json =
    if valOf (J.toBool json) then "true" else "false"
and arrayToJSONString json =
    let val elements = valOf (J.elements json)
    in
	String.concat ["[", String.concatWith "," (map toJSONString elements), "]"]
    end
and objectToJSONString json =
    let val members = ListPair.unzip (valOf (J.members json))
	fun pairToString (name, value) =
	    String.concat [stringToJSONString (J.string name), ":", toJSONString value]
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
