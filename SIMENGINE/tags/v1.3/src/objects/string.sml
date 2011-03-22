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

structure StringObjects =
struct

val symbol = Symbol.symbol

(* Returns a library function invocation. *)
fun libfun name args =
    KEC.LIBFUN (symbol name, KEC.TUPLE args)

(* Returns a SYMBOL expression with the given name. *)
fun sym s = KEC.SYMBOL (symbol s)
	    
fun stringVal (KEC.LITERAL (KEC.CONSTSTR s)) = s
  | stringVal exp = raise DynException.TypeMismatch ("Expected a string but received " ^ (PrettyPrint.kecexp2nickname exp))

fun string_length obj = 
    Objects.method "length" nil
		   (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (String.size (stringVal obj)))))


fun string_tonumber obj = 
    Objects.method "tonumber" nil
		   (case Real.fromString (stringVal obj) of
			SOME r => KEC.LITERAL (KEC.CONSTREAL r)
		      | NONE => KEC.UNIT)
			  
fun string_first obj =
    Objects.method "first" nil
		   (KEC.LITERAL (KEC.CONSTSTR (case stringVal obj of
						   "" => ""
						 | s => (String.str (String.sub (s,0))))))
    
fun string_rest obj = 
    Objects.method "rest" nil
		   (KEC.LITERAL (KEC.CONSTSTR (case stringVal obj of
						   "" => ""
						 | s => ((implode o tl o explode) s))))



fun string_substring obj = 
    Objects.method "substring" [(symbol "pos", KEC.TYPE (symbol "Number")),
				(symbol "len", KEC.TYPE (symbol "Number"))]
		   (libfun "substring" [obj, sym "pos", sym "len"])

fun string_contains obj = 
    Objects.method "contains" [(symbol "substr", KEC.TYPE (symbol "String"))]
		   (libfun "str_contains" [obj, sym "substr"])
    
fun string_startsWith obj = 
    Objects.method "startsWith" [(symbol "substr", KEC.TYPE (symbol "String"))]
		   (libfun "str_startsWith" [obj, sym "substr"])

val methods = Env.new()

val _ = app (fn (n,m) => (Env.add ((symbol n, m), methods); ()))
	    [("length", string_length)(*,
	     ("tonumber", string_tonumber),
	     ("first", string_first),
	     ("rest", string_rest),
	     ("substring", string_substring),
	     ("contains", string_contains),
	     ("startsWith", string_startsWith)*)]
	
end
