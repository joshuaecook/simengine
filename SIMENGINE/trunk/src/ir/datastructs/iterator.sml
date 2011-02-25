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

signature ITERATOR =
sig
    (* Terms are indexed by an iterator to refer to
     * a specific value or set of values. *)
    datatype iteratorindex
      (* An index relative to the current value.
       * The (RELATIVE 0) index is the common means of 
       * referring to the "now" value. *)
      = RELATIVE of int
      (* The (ABSOLUTE 0) index is the common means of 
       * referring to the initial value. *)
      | ABSOLUTE of int
      | RANGE of (int * int)
      | LIST of int list
      | ALL
				     
    type iterator = (Symbol.symbol * iteratorindex)

    (* Useful functions *)
    val iterator2str : iterator -> string
    val iterators2str : iterator list -> string
    val iterator2c_str : iterator -> string (* C writer for iterator *)
    val iterators2c_str : iterator list -> string (* C writer for multiple iterator *)
    val iterator2mathematica_str : iterator -> string (* C writer for iterator *)
    val iterators2mathematica_str : iterator list -> string (* C writer for multiple iterator *)
    val iter_equiv : (iterator * iterator) -> bool

    val preProcessOf : string -> Symbol.symbol  
    val inProcessOf : string -> Symbol.symbol  
    val postProcessOf : string -> Symbol.symbol  
    val updateOf : string -> Symbol.symbol

end
structure Iterator : ITERATOR =
struct

val i2s = Util.i2s

fun addPrefix prefix iter = 
    let val prefix' = prefix ^ "_"
    in if (String.isPrefix Util.commonPrefix iter) then
	   Symbol.symbol (Util.commonPrefix ^ prefix' ^ (Util.removePrefix iter))
       else
	   Symbol.symbol (prefix' ^ iter)
    end

fun eventOf iter = addPrefix "event" iter
fun preProcessOf iter = addPrefix "preProcess" iter
fun inProcessOf iter = addPrefix "inProcess" iter
fun postProcessOf iter = addPrefix "postProcess" iter
fun updateOf iter = addPrefix "update" iter

datatype iteratorindex = ALL
		       | ABSOLUTE of int
		       | RELATIVE of int
		       | RANGE of (int * int)
		       | LIST of int list

type iterator = (Symbol.symbol * iteratorindex)

fun iterator2str (iterator as (sym, i))= 
    let
	val str = (*Util.removePrefix*) (Symbol.name sym)
    in
	case i
	 of ALL => str ^ "=:"
	  | ABSOLUTE v => str ^ "=" ^ (i2s v)
	  | RELATIVE v => str ^ (if v = 0 then
				     ""
				 else if v < 0 then 
				     i2s v 
				 else 
				     "+" ^ (i2s v))
	  | RANGE (v1, v2) => str ^ "=" ^ (i2s v1) ^ ":" ^ (i2s v2)
	  | LIST l => str ^ "=(" ^ (String.concatWith "," (map i2s l)) ^ ")"
    end

fun iterators2str iterators =
    "[" ^ (String.concatWith "," (map iterator2str iterators)) ^ "]"


fun iterator2c_str (iterator as (sym, i)) =
    let
	val str = Symbol.name sym
    in
	case i of
	    ALL => ""
	  | ABSOLUTE i => (i2s i)
	  | RELATIVE i => str ^ (if i = 0 then "" else "+" ^ (i2s i))
	  | RANGE _ => DynException.stdException("Currently, an iterator range can't be specified.  Instead, use one or more symbols with absolute indices.", "Iterator.iterator2c_str", Logger.INTERNAL)
	  | LIST _ => DynException.stdException("Currently, an iterator list can't be specified.  Instead, use one or more symbols with absolute indices.", "Iterator.iterator2c_str", Logger.INTERNAL)
    end

fun iterators2c_str iterators =
    if List.length iterators > 0 then
	String.concat (map (fn(iter)=> "[" ^ (iterator2c_str iter) ^ "]") iterators)
	(*"[" ^ (String.concatWith "," (map iterator2c_str iterators)) ^ "]"*)
    else
	""

fun iterator2mathematica_str (iterator as (sym, i)) =
    let
	val str = Symbol.name sym
    in
	case i of
	    ALL => ""
	  | ABSOLUTE i => (i2s i)
	  | RELATIVE i => str ^ (if i = 0 then "" else "+" ^ (i2s i))
	  | RANGE _ => DynException.stdException("Currently, an iterator range can't be specified.  Instead, use one or more symbols with absolute indices.", "Iterator.iterator2c_str", Logger.INTERNAL)
	  | LIST _ => DynException.stdException("Currently, an iterator list can't be specified.  Instead, use one or more symbols with absolute indices.", "Iterator.iterator2c_str", Logger.INTERNAL)
    end

fun iterators2mathematica_str iterators =
    if List.length iterators > 0 then
	String.concat (map (fn(iter)=> "[" ^ (iterator2mathematica_str iter) ^ "]") iterators)
	(*"[" ^ (String.concatWith "," (map iterator2c_str iterators)) ^ "]"*)
    else
	""



fun iter_equiv (i1 as (s1, t1), i2 as (s2, t2)) =
    (* check the symbol *)
    s1 = s2
    andalso
    (case (t1, t2)
      of (ALL, ALL) => true
       | (ABSOLUTE a, ABSOLUTE b) => a=b
       | (RELATIVE a, RELATIVE b) => a=b
       | (RANGE (a1,a2), RANGE (b1,b2)) => (a1=b1) andalso (a2=b2)
       | (LIST l1, LIST l2) => (length l1) = (length l2)
			       andalso 
			       List.all (fn(a,b)=>a=b) (ListPair.zip (l1, l2))
       | _ => false)
    
end
