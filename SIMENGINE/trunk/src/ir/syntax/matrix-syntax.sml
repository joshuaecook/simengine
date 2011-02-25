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

structure MatrixSyntax: sig
(* Serialization and deserialization for Matrix data. *)

val toJSON: 'a Matrix.matrix -> JSON.json
(* TODO implement fromJSON *)

end = struct

open JSON
open JSONExtensions

val int = int o IntInf.fromInt

fun toJSON (m as ref (Matrix.DENSE {calculus, data})) =
    let val (rows, columns) = Matrix.size m
    in
	JSONTypedObject ("Matrix.DENSE", 
			 object [("columns", int columns),
				 ("rows", int rows),
				 ("FIXME", null)])
    end
  | toJSON (m as ref (Matrix.BANDED {calculus, data, nrows, ncols, lowerbw, upperbw})) = 
    JSONTypedObject ("Matrix.BANDED", 
		     object [("columns", int ncols),
			     ("rows", int nrows),
			     ("bandwidth", object [("upper", int upperbw), 
						   ("lower", int lowerbw)]),
			     ("data", array (map (array o (Array.foldr (fn (x, xs) => ((#toJSON calculus) x) :: xs) nil)) (Matrix.toBands m)))])

end
