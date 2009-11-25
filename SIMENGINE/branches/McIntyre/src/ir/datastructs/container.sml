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

(* Matrix mappings *)
val matrix2rows : matrix -> array list
val matrix2columns : matrix -> array list
val listexp2listarray : Exp.exp list -> array list
val matrix2size : matrix -> (int * int)

(* Container mappings *)
val container2elements : Exp.container -> Exp.exp list

(* Matrix creation *)
val zeros_matrix : (int * int) -> Exp.exp
val identity_matrix : (int) -> Exp.exp
val transpose : Exp.exp -> Exp.exp

(* Matrix operations *)
val isSquare : matrix -> bool
val getBand : matrix -> int -> array
val findBandwidth : matrix -> (int * int)
val dense2bandedmatrix : matrix -> array list

end

structure Container:CONTAINER =
struct

type matrix = Exp.exp Array2.array
type array = Exp.exp Array.array

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

fun allzero a =
    let
	fun isZero (Exp.TERM t) = Term.isZero t
	  | isZero _ = false
    in
	Array.foldl (fn(item, zero)=>zero andalso isZero item) true a
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

fun isSquare m =
    let
	val (rows, cols) = matrix2size m
    in
	rows = cols
    end

(* getBand will pull out a particular diagonal band from the matrix. 
  When num is zero, the main diagonal is pulled.  When num is greater than zero, bands
  are taken from the upper triangular matrix.  When num is less than zero, bands are
  taken from the lower triangular matrix. *)
fun getBand m num =
    let
	val (rows, cols) = matrix2size m
	val max_dim = if rows > cols then rows else cols
	val _ = if Int.abs num + 1 >= rows orelse Int.abs num + 1 >= cols then
		    DynException.stdException("Trying to grab a band that is outside of the matrix's dimensions",
					      "Container.getBand",
					      Logger.INTERNAL)
		else
		    ()
	val all_indices = map 
			      (fn(i)=>if num > 0 then
					  (i, i+num)
				      else if num < 0 then
					  (i-num, i)
				      else (* num == 0 *)
					  (i, i)) 
			      (List.tabulate (max_dim, fn(x)=>x))
	val valid_indices = List.filter (fn(r,c)=> r >= 0 andalso r < rows andalso 
						   c >= 0 andalso c < cols) all_indices

	val items = map 
			(fn(i,j)=>Array2.sub(m, i, j))
			valid_indices
    in
	list2array items
    end

(* findBandwidth will determine the number of upper and lower non-zero bands that encompass the matrix *)
fun findBandwidth m =
    if isSquare m then
	let
	    val (dim, _) = matrix2size m
	    val half_bands = dim-1
	    val total_bands = 1+2*(half_bands)

	    (* start with upper bands *) 
	    val upper_band_list = List.tabulate (half_bands, (fn(x)=>half_bands-x))
	    val (upper_half_bw,_) = foldl 
					(fn(test_band, (bw, found_non_zero))=>
					   if found_non_zero then
					       (bw, true)
					   else
					       let
						   val a = getBand m test_band
					       in
						   if allzero a then
						       (test_band-1, false)
						   else
						       (test_band, true)
					       end
					)
					(half_bands, false)
					upper_band_list
					
	    (* lower half bands *)
	    val lower_band_list = List.tabulate (half_bands, (fn(x)=> ~(half_bands-x)))
	    val (lower_half_bw,_) = foldl 
					(fn(test_band, (bw, found_non_zero))=>
					   if found_non_zero then
					       (bw, true)
					   else
					       let
						   val a = getBand m test_band
					       in
						   if allzero a then
						       (~test_band-1, false)
						   else
						       (~test_band, true)
					   end
					)
					(half_bands, false)
					lower_band_list
	in
	    (upper_half_bw, lower_half_bw)
	end
    else
	DynException.stdException("Only supports square matrices", "Container.findBandwidth", Logger.INTERNAL)

(* convert a dense matrix to a banded matrix - this will be a list of arrays *)
fun dense2bandedmatrix m =
    if isSquare m then
	let
	    val (upper_bw,lower_bw) = findBandwidth m

	    (* grab the diagonal *)
	    val diag = getBand m 0
		       
	    (* grab the upper bands *)
	    val upper_bands = List.tabulate (upper_bw, fn(x)=> getBand m (x+1))

	    (* grab the lower bands *)
	    val lower_bands = List.tabulate (lower_bw, fn(x)=> getBand m (~(x+1)))
	in
	    upper_bands @ [diag] @ lower_bands
	end
    else
	DynException.stdException("Only supports square matrices", "Container.dense2bandedmatrix", Logger.INTERNAL)


end

