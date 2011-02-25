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

(* library of SML functions used in Sail *)

fun sort_compare lessthan list =
    let
	fun insert lessthan n nil =
	    [n]
	  | insert lessthan n (n'::rest) =
	    if lessthan(n, n') then
		n::n'::rest
	    else
		n'::(insert lessthan n rest)
		
	fun isort lessthan alist nil = alist
	  | isort lessthan alist (n::rest) =
	    isort lessthan (insert lessthan n alist) rest
		  
    in
	isort lessthan [] list
    end

(* first for arrays *)
val array_null = List.null
fun array_split [] = ([], [])
  | array_split [item] = ([item], [])
(*  | array_split (item::rest) = ([item], rest)*)
  | array_split (items) = 
    let
	val len = List.length items
	val half_len = Int.div (len, 2)
	val numbered_items = ListPair.zip (items, List.tabulate (len, fn(x)=>x))
	val (lower_half, upper_half) = List.partition (fn(_, n)=> n<=half_len) numbered_items
	fun clean items = map (fn(i,_)=>i) items
    in
	(clean lower_half, clean upper_half)
    end
val array_sort = sort_compare (op <)
val array_concat = (op @)

(* now for pairs *)
fun pair_one (a,b) = a
fun pair_two (a,b) = b
;
