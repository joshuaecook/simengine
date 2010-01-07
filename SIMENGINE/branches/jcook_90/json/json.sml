(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

signature JSON = sig
    (* The type of JSON values. *)
    type json
    (* The type of JSON value types. *)
    datatype value = JS_NULL | JS_FALSE | JS_TRUE | JS_OBJECT | JS_ARRAY | JS_STRING | JS_NUMBER

    (* Indicates the type of a JSON value. *)
    val value: json -> value

    (* The null value. *)
    val null: json

    (* Injects a primitive value into a JSON value. *)
    val bool: bool -> json
    val int: IntInf.int -> json
    val real: LargeReal.real -> json
    val string: string -> json

    (* Constructs a JSON container. *)
    val array: json list -> json
    val object: (string * json) list -> json

    val isNull: json -> bool

    (* Returns SOME primitive value iff the JSON value represents a value of that type. Returns NONE otherwise. *)
    val toBool: json -> bool option
    val toInt: json -> IntInf.int option
    val toReal: json -> LargeReal.real option
    val toString: json -> string option

    val isArray: json -> bool
    (* Returns SOME nth element of a JSON array.
     * Returns NONE if the value is not a JSON array
     * or if the index exceeds the array dimensions. *)
    val nth: json * int -> json option
    val elements: json -> json list option

    val isObject: json -> bool
    (* Returns SOME member value of a JSON object.
     * Returns NONE if the value is not a JSON object
     * or if the object does not have such a member. *)
    val member: json * string -> json option
    val members: json -> (string * json) list option
end


structure JSON:> JSON = struct
datatype value = JS_NULL | JS_FALSE | JS_TRUE | JS_OBJECT | JS_ARRAY | JS_STRING | JS_NUMBER
datatype json =
	 NULL
       | TRUE
       | FALSE
       | INT of IntInf.int
       | REAL of LargeReal.real
       | STRING of string
       | ARRAY of json list
       | OBJECT of (string * json) list

val value = 
 fn NULL => JS_NULL
  | TRUE => JS_TRUE
  | FALSE => JS_FALSE
  | INT _ => JS_NUMBER
  | REAL _ => JS_NUMBER
  | STRING _ => JS_STRING
  | ARRAY  _ => JS_ARRAY
  | OBJECT _ => JS_OBJECT

val null = NULL

val bool = fn true => TRUE | _ => FALSE
val int = INT
val real = REAL
val string = STRING

val array = ARRAY
val object = OBJECT

val isNull = fn NULL => true | _ => false

val toBool = fn TRUE => SOME true | FALSE => SOME false | _ => NONE
val toInt = fn INT z => SOME z | _ => NONE
val toReal = fn REAL r => SOME r | _ => NONE
val toString = fn STRING s => SOME s | _ => NONE

val isArray = fn ARRAY _ => true | _ => false

fun nth (ARRAY elems, n) = 
    (SOME (List.nth (elems, n))
     handle Subscript => NONE)
  | nth _ = NONE

val elements = fn ARRAY elems => SOME elems | _ => NONE

val isObject = fn OBJECT _ => true | _ => false

fun member (OBJECT members, name) =
    (case List.find (fn (k,v) => name = k) members
      of SOME (k,v) => SOME v
       | _ => NONE)
  | member _ = NONE

val members = fn OBJECT members => SOME members | _ => NONE
end
