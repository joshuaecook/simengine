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

structure Unique =
struct

type uniqueid = int

val seed = ref 0

fun genid () =
    !seed before seed := !seed + 1

fun unique base =
    base ^ "_" ^ (Int.toString (genid()))

end


structure UniqueTable=

struct

structure H = ListMapFn (struct
			 type ord_key = Unique.uniqueid
			 val compare = Int.compare
			 end)
					    
  type 'a table = 'a H.map
  val empty = H.empty
  fun enter (t, k, a) = H.insert(t, k, a)
  fun look(t, k) = H.find(t,k)
  fun remove(t, k) = H.remove(t, k)
  fun numItems(t) = H.numItems(t)
  fun listKeys (t) = (H.listKeys (t))
  fun listItems (t) = H.listItems (t)
  fun map(f) = H.map (f)
  fun priorityunion (tab1, tab2) = H.unionWith (fn(a,_) => a) (tab1, tab2) 

(*
  type 'a table = (string * 'a) list
  val empty = nil
  fun enter (t,k,a) = (k,a) :: t
  fun look (t,k) = case (List.find (fn(k',a') => k = k') t) of
		       SOME (k,a) => SOME a
		     | NONE => NONE
  fun remove (t,k) = List.filter (fn(k',a') => k <> k') t
  fun numItems (t) = length t
  fun listKeys (t) = map (fn(k,a) => k) t
  fun listItems (t) = map (fn(k,a) => a) t
  val map = map
  fun priorityunion (tab1, tab2) = tab1 @ tab2*)
end
