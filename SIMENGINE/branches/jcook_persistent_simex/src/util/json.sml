signature JSON =
sig

    type json_value

    (* The literal null value. *)
    val js_null : json_value
    (* The literal truth value. *)
    val js_true : json_value
    (* The literal falsity value. *)
    val js_false : json_value
    (* Constructs a boolean value. *)
    val js_boolean : bool -> json_value
    (* Constructs a string value *)
    val js_string : string -> json_value
    (* Constructs an integer numeric value. *)
    val js_int : int -> json_value
    (* Constructs a floating-point numeric value. *)
    val js_float : real -> json_value
    (* Constructs an array of values. *)
    val js_array : json_value list -> json_value
    (* Constructs an object of key-value associations. *)
    val js_object : (string * json_value) list -> json_value

    (* Returns a string representation of a JSON datum. *)
    val to_json_string : json_value -> string

    val output : (TextIO.outstream * json_value) -> unit
end

structure mlJS : JSON =
struct

exception JSON_Error of string

datatype json_null = Null
datatype json_boolean = True | False
datatype json_string = String of string
datatype json_number = Int of int | Real of real

type 'a json_pair = json_string * 'a

datatype 'a json_array = Array of 'a list
datatype 'a json_object = Object of ('a json_pair) list

datatype json_value =
	 NULL of json_null
       | BOOLEAN of json_boolean
       | STRING of json_string
       | NUMBER of json_number
       | ARRAY of json_value json_array
       | OBJECT of json_value json_object

fun numeric_to_json_string num = String.translate (String.str o (fn #"~" => #"-" | c => c)) num

fun to_json_string (NULL _) = "null"
  | to_json_string (BOOLEAN True) = "true"
  | to_json_string (BOOLEAN False) = "false"
  | to_json_string (NUMBER (Int i)) = numeric_to_json_string (Int.toString i)
  | to_json_string (NUMBER (Real r)) = 
    if Real.isFinite r then numeric_to_json_string (Real.toString r)
    (* Although JavaScript supports NaN and Infinity, they are not part of the JSON spec and are encoded here as strings. 
     * Their true value can be recovered in JavaScript by calling parseFloat(). *)
    else if Real.isNan r then string_to_json_string (String "NaN")
    else if 0.0 > r then string_to_json_string (String "-Infinity")
    else string_to_json_string (String "Infinity")
  | to_json_string (STRING s) = string_to_json_string s
  | to_json_string (ARRAY (Array vs)) = 
    "[" ^ (String.concatWith "," (map to_json_string vs)) ^ "]"
  | to_json_string (OBJECT (Object pairs)) = 
    "{" ^ (String.concatWith "," (map pair_to_json_string pairs)) ^ "}"
and string_to_json_string (String s) = 
    "\"" ^ String.toString s ^ "\""
and pair_to_json_string (key, value) = 
    (string_to_json_string key) ^ ":" ^ (to_json_string value)

val js_null = NULL Null
val js_true = BOOLEAN True
val js_false = BOOLEAN False

fun js_boolean true = BOOLEAN True
  | js_boolean false = BOOLEAN False

fun js_string s = STRING (String s)

fun js_int i = NUMBER (Int i)
fun js_float r = NUMBER (Real r)

fun js_array vs = ARRAY (Array vs)

(*fun js_object kvs = 
    let
	fun kv2pair (STRING key, value) = (key,value)
	  | kv2pair _ = raise JSON_Error "JSON object keys must be strings."
    in OBJECT (Object (map kv2pair kvs)) end*)

fun js_object (kvs: (string * json_value) list) : json_value = 
    let
	fun kv2pair (key, value) = (String key,value)
(*	  | kv2pair _ = raise JSON_Error "JSON object keys must be strings."*)
    in OBJECT (Object (map kv2pair kvs)) end

fun output (stream, value) =
    TextIO.output (stream, to_json_string value)

end
