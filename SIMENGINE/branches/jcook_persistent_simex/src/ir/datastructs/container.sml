signature CONTAINER =
sig

type matrix = Exp.exp Array2.array
type array = Exp.exp Array.array

(* Vector routines *)
val vector2list : Exp.exp Vector.vector -> Exp.exp list
val list2vector : Exp.exp list -> Exp.exp Vector.vector
val vectors2matrix : Exp.exp Vector.vector list -> matrix
val vector2array : Exp.exp Vector.vector -> array

(* Array mappings *)
val array2list : array -> Exp.exp list
val list2array : Exp.exp list -> array
val rows2matrix : array list -> matrix
val columns2matrix : array list -> matrix
val array2size : array -> int
val exparray2array : Exp.exp -> array
val array2exparray : array -> Exp.exp

(* Matrix mappings *)
val matrix2rows : matrix -> array list
val matrix2columns : matrix -> array list
val listexp2listarray : Exp.exp list -> array list
val matrix2size : matrix -> (int * int)
val expmatrix2matrix : Exp.exp -> matrix
val matrix2expmatrix : matrix -> Exp.exp
val matrixmap : ((Exp.exp * int * int) -> 'a) -> matrix -> 'a list

(* Container mappings *)
val container2elements : Exp.container -> Exp.exp list

(* Matrix creation *)
val zeros_matrix : (int * int) -> Exp.exp
val zeros_array : int -> Exp.exp

val identity_matrix : (int) -> Exp.exp
val transpose : Exp.exp -> Exp.exp


end

structure Container:CONTAINER =
struct

type matrix = Exp.exp Array2.array
type array = Exp.exp Array.array

val i2s = Util.i2s

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


fun array2size a = Array.length a
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

fun array2exparray a =
    Exp.CONTAINER (Exp.ARRAY a)

fun zeros_matrix (rows,cols) =
    matrix2expmatrix (Array2.array (rows, cols, zero))

fun zeros_array cols =
    array2exparray (Array.array (cols, zero))

fun exparray2array (Exp.CONTAINER (Exp.ARRAY a)) = a
  | exparray2array _ = DynException.stdException("Non-array passed in", 
						   "Container.exparray2array", 
						   Logger.INTERNAL)


fun expmatrix2matrix (Exp.CONTAINER (Exp.MATRIX m)) = m
  | expmatrix2matrix _ = DynException.stdException("Non-matrix passed in", 
						   "Container.expmatrix2matrix", 
						   Logger.INTERNAL)


fun matrixmap mapfun m =
    let
	val rows = matrix2rows m

    in
	Util.flatmap 
	    (fn(r,i)=> map (fn(e,j)=> mapfun (e, i, j)) (StdFun.addCount (array2list r)))
	    (StdFun.addCount rows)
    end

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

