(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure MatrixSyntax: sig
(* Serialization and deserialization for Matrix data. *)

val toJSON: 'a Matrix.matrix -> JSON.json
(* TODO implement fromJSON *)

end = struct
open JSON
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
