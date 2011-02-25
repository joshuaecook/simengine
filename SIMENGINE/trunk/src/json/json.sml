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

structure JSON:> JSON = struct
datatype jsType = JS_NULL | JS_FALSE | JS_TRUE | JS_OBJECT | JS_ARRAY | JS_STRING | JS_NUMBER
datatype json =
	 NULL
       | TRUE
       | FALSE
       | INT of IntInf.int
       | REAL of LargeReal.real
       | STRING of string
       | ARRAY of json list
       | OBJECT of (string * json) list

val jsType = 
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

val boolVal = valOf o toBool
val intVal = valOf o toInt
val realVal = valOf o toReal
val stringVal = valOf o toString

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

fun memberDefault (object, name, {default}) =
    getOpt (member (object, name), default)

fun memberValue (object as OBJECT _, name, dejs) =
    (case member (object, name)
      of SOME v => dejs v | NONE => NONE)
  | memberValue _ = NONE

fun memberVal (object, name, dejs) =
    dejs (valOf (member (object, name)))

val members = fn OBJECT members => SOME members | _ => NONE
end
