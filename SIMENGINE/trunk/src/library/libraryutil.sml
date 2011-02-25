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

structure LibraryUtil =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_error (Printer.$ msg)

val nick = PrettyPrint.kecexp2nickname


(* Many of the numeric functions have the same (real -> real) signature with a simple operation.
   This is a helper function for creating library methods of that type. *)
fun realfun (f: (real -> real)) = 
 fn [KEC.LITERAL (KEC.CONSTREAL r)] 
    => KEC.LITERAL (KEC.CONSTREAL (f r))
  | [a] 
    => raise TypeMismatch ("expected a number but received " ^ (nick a))
  | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

(* As above, but with the signature (real * real -> real) *)
fun realsfun (f: (real * real -> real)) =
 fn [KEC.LITERAL (KEC.CONSTREAL r1), KEC.LITERAL (KEC.CONSTREAL r2)] 
    => KEC.LITERAL (KEC.CONSTREAL (f (r1,r2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 numbers but received " ^ (nick a) ^ " and " ^ (nick b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, but with the signature (real * real -> bool) *)
fun reals2boolfun (f: (real * real -> bool)) = 
 fn [KEC.LITERAL (KEC.CONSTREAL r1), KEC.LITERAL (KEC.CONSTREAL r2)] 
    => KEC.LITERAL (KEC.CONSTBOOL (f (r1,r2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 numbers but received " ^ (nick a) ^ " and " ^ (nick b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, with with the signature (string -> string) *)
fun strfun (f: (string -> string)) =
 fn [KEC.LITERAL (KEC.CONSTSTR s)] => (KEC.LITERAL o KEC.CONSTSTR o f) s
  | [a] => raise TypeMismatch ("expected a string but received " ^ (nick a))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, with with the signature (string -> real) *)
fun strToRealFun (f: (string -> real)) =
 fn [KEC.LITERAL (KEC.CONSTSTR s)] => (KEC.LITERAL o KEC.CONSTREAL o f) s
  | [a] => raise TypeMismatch ("expected a string but received " ^ (nick a))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, with with the signature (string * string -> string) *)
fun strsfun (f: (string * string -> string)) =
 fn [KEC.LITERAL (KEC.CONSTSTR s1), KEC.LITERAL (KEC.CONSTSTR s2)] 
    => KEC.LITERAL (KEC.CONSTSTR (f (s1, s2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 strings but received " ^ (nick a) ^ " and " ^ (nick b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, with with the signature (string * string -> bool) *)
fun strs2boolfun (f: (string * string -> bool)) =
 fn [KEC.LITERAL (KEC.CONSTSTR s1), KEC.LITERAL (KEC.CONSTSTR s2)] 
    => KEC.LITERAL (KEC.CONSTBOOL (f (s1, s2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 strings but received " ^ (nick a) ^ " and " ^ (nick b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}


fun unitToStringFun (f: (unit -> string)) =
 fn nil => (KEC.LITERAL o KEC.CONSTSTR o f) ()
  | args => raise IncorrectNumberOfArguments {expected=0, actual=(length args)}

fun optStrToStringFun (f: (string option -> string)) = 
 fn [KEC.LITERAL (KEC.CONSTSTR s)] => (KEC.LITERAL o KEC.CONSTSTR o f o SOME) s
  | [a] => raise TypeMismatch ("expected a string but received " ^ (nick a))
  | nil => (KEC.LITERAL o KEC.CONSTSTR o f) NONE
  | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

local
    fun allStrings l = List.all (fn(i)=>case i of KEC.LITERAL (KEC.CONSTSTR _) => true | _ => false) l
    fun kecStringListToStringList l = List.mapPartial (fn(i)=> case i of KEC.LITERAL (KEC.CONSTSTR s) => SOME s | _ => NONE) l
in
fun strListToStringFun (f: (string list -> string)) =
 fn l => if allStrings l then
	     (KEC.LITERAL o KEC.CONSTSTR o f o kecStringListToStringList) l
	 else
	     raise TypeMismatch ("expected all strings")

fun strListToKEC (f: (string list -> KEC.exp)) =
 fn l => if allStrings l then
	     (f o kecStringListToStringList) l
	 else
	     raise TypeMismatch ("expected all strings")

end 

end
