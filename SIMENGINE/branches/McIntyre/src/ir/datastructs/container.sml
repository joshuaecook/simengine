signature CONTAINER =
sig

(* Vector routines *)
val vector2list : Exp.exp Vector.vector -> Exp.exp list
val list2vector : Exp.exp list -> Exp.exp Vector.vector
val vectors2matrix : Exp.exp Vector.vector list -> Exp.exp Array2.array
val vector2array : Exp.exp Vector.vector -> Exp.exp Array.array

(* Array mappings *)
val array2list : Exp.exp Array.array -> Exp.exp list
val list2array : Exp.exp list -> Exp.exp Array.array
val rows2matrix : Exp.exp Array.array list -> Exp.exp Array2.array
val columns2matrix : Exp.exp Array.array list -> Exp.exp Array2.array

(* rows2matrix and columns2matrix *)

(* Matrix mappings *)
val matrix2rows : Exp.exp Array2.array -> Exp.exp Array.array list
val matrix2columns : Exp.exp Array2.array -> Exp.exp Array.array list
val listexp2listarray : Exp.exp list -> Exp.exp Array.array list
val matrix2size : Exp.exp Array2.array -> (int * int)

(* Container mappings *)
val container2elements : Exp.container -> Exp.exp list

(* Matrix creation *)
val zeros_matrix : (int * int) -> Exp.exp
val identity_matrix : (int) -> Exp.exp
val transpose : Exp.exp -> Exp.exp

end

structure Container:CONTAINER =
struct

fun vector2list v = map 
			(fn(i)=>Vector.sub (v,i)) 
			(List.tabulate (Vector.length v, (fn(x)=>x)))
fun array2list v = map 
			(fn(i)=>Array.sub (v,i)) 
			(List.tabulate (Array.length v, (fn(x)=>x)))
fun list2vector l = Vector.fromList l
fun list2array l = Array.fromList l

fun vector2array v = list2array (vector2list v)

fun matrix2rows m = map 
			(fn(i)=>vector2array (Array2.row (m, i)))
			(List.tabulate (Array2.nRows m, (fn(x)=>x)))

fun matrix2columns m = map 
			   (fn(i)=>vector2array (Array2.column (m, i)))
			   (List.tabulate (Array2.nCols m, (fn(x)=>x)))

fun matrix2list m = Util.flatmap 
			array2list
			(matrix2rows m)

fun matrix2size m = Array2.dimensions m

fun listexp2listarray l =
    let
	fun exp2array (Exp.CONTAINER (Exp.ARRAY a)) = a
	  | exp2array _ = 
	    DynException.stdException("Unexpected expression",
				      "Container.listexp2listarray.exp2array",
				      Logger.INTERNAL)
    in
	map exp2array l
    end

fun container2elements (Exp.EXPLIST l) = l
  | container2elements (Exp.ARRAY a) = array2list a
  | container2elements (Exp.MATRIX m) = matrix2list m

val zero = Exp.TERM (Exp.INT 0)
val one = Exp.TERM (Exp.INT 1)

fun matrix2expmatrix m =
    Exp.CONTAINER (Exp.MATRIX m)

fun zeros_matrix (rows,cols) =
    matrix2expmatrix (Array2.array (rows, cols, zero))

fun expmatrix2matrix (Exp.CONTAINER (Exp.MATRIX m)) = m
  | expmatrix2matrix _ = DynException.stdException("Non-matrix passed in", 
						   "Container.expmatrix2matrix", 
						   Logger.INTERNAL)


fun identity_matrix (size) = 
    let
	val m_exp = zeros_matrix (size, size)
	val m = expmatrix2matrix m_exp
	val _ = app	    
		    (fn(i)=>Array2.update (m, i, i, one))
		    (List.tabulate (size, (fn(x)=>x)))
    in
	matrix2expmatrix m
    end

fun transpose exp =
    let
	val m = expmatrix2matrix exp
	val (rows, cols) = matrix2size m

	val m' = expmatrix2matrix (zeros_matrix (cols, rows))
	(* chose a region encompassing the entire matrix *)
	val m_region = {base=m, row=0, col=0, nrows=NONE, ncols=NONE}
	(* update m', flipping the indices *)
	val _ = Array2.appi Array2.RowMajor (fn(i,j,e)=>Array2.update (m', j, i, e)) m_region
    in
	matrix2expmatrix m'
    end


fun vectors2matrix vectors = 
    let
	val lists = map vector2list vectors
    in
	Array2.fromList lists
    end

fun rows2matrix vectors = 
    let
	val lists = map array2list vectors
    in
	Array2.fromList lists
    end

fun columns2matrix vectors = 
    let
	val lists = map array2list vectors
	val m = Array2.fromList lists
    in
	(expmatrix2matrix o transpose o matrix2expmatrix) m
    end



end
