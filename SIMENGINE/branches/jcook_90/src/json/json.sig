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

    (* Returns the primitive value of a JSON value.
     * Raises Option if the JSON value does not represent a value of that type. *)
    val boolVal: json -> bool
    val intVal: json -> IntInf.int
    val realVal: json -> LargeReal.real
    val stringVal: json -> string

    val isArray: json -> bool
    (* Returns SOME nth element of a JSON array.
     * Returns NONE if the value is not a JSON array
     * or if the index exceeds the array dimensions. *)
    val nth: json * int -> json option
    (* Returns SOME list of elements if the value is a JSON array.
     * Returns NONE if the value is not an array. *)
    val elements: json -> json list option

    val isObject: json -> bool
    (* Returns SOME member value of a JSON object.
     * Returns NONE if the value is not a JSON object
     * or if the object does not have such a member. *)
    val member: json * string -> json option
    (* Returns a member value of a JSON object
     * or the given default value if the object has no such member. *)
    val memberDefault: json * string * {default:json} -> json
    val memberValue: json * string * (json -> 'a option) -> 'a option
    val memberVal: json * string * (json -> 'a) -> 'a
    (* Returns SOME list of member name/value pairs if the value is a JSON object.
     * Returns NONE if the value is not an object. *)
    val members: json -> (string * json) list option
end
