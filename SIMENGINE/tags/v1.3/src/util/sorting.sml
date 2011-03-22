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

signature SORTING = sig

(* Returns a new list having its items in ascending order 
 * according to the given ordering function. *)
val sorted: ('a * 'a -> order) -> 'a list -> 'a list

end

structure Sorting: SORTING = struct

fun insertionSort order =
    let fun insert (x, nil) = [x]
	  | insert (x, y::ys) = 
	    (case order (x, y)
	      of LESS => x :: y :: ys
	       | EQUAL => x :: y :: ys
	       | GREATER => y :: (insert (x, ys)))
    in
	List.foldr insert nil
    end


val sorted = insertionSort

end
