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

signature TERMPROCESS = 
sig

    (* iterator related term functions *)
    val symbol2temporaliterator : Exp.term -> Iterator.iterator option
    val symbol2spatialiterators : Exp.term -> Iterator.iterator list

end
structure TermProcess : TERMPROCESS =
struct

fun symbol2temporaliterator term = 
    let
	val iterators = CurrentModel.iterators()
(*	val _ = Util.log("Global iterators = " ^ (Util.symlist2s (map #1 iterators)))*)
    in
	case term of
	    Exp.SYMBOL (sym, props) => 
	    (case Property.getIterator props of
		 SOME iters => 
		 List.find 
		     (fn (sym, _) => List.exists (fn(sym',_)=> sym = sym') iterators) 
		     iters
	       | NONE => NONE)
	  | _ => NONE
    end

fun symbol2spatialiterators term = 
    case symbol2temporaliterator term of
	SOME (itersym,_) => 
	 (case term of
	      Exp.SYMBOL (_, props) => 
	      (case Property.getIterator props of
		   SOME iters => List.filter (fn(sym', _)=> not (itersym = sym')) iters
		 | NONE => [])
	    | _ => [])
      | NONE => (case term of
		     Exp.SYMBOL (_, props) => 
		     (case Property.getIterator props of
			  SOME iters => iters
			| NONE => [])
		   | _ => [])


end
