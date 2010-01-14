signature MATRIX =
sig

type 'a array2d = 'a Array2.array
type 'a array1d = 'a Array.array

(* Matrix Datatype - describes two implementations, a banded version and a dense version *)
datatype 'a matrixtype = BANDED of {ncols:int,
				    nrows:int,
				    upperbw:int,
				    lowerbw:int,
				    calculus: 'a Calculus.calculus,
				    data: 'a array1d list}
		       | DENSE of {calculus: 'a Calculus.calculus,
				   data: ('a array2d)}
type 'a matrix = 'a matrixtype ref

(* Matrix creation *)
val zeros : 'a Calculus.calculus -> (int * int) -> 'a matrix
val identity : 'a Calculus.calculus -> int -> 'a matrix

(* Matrix operations *)
val size : 'a matrix -> (int * int)
val isSquare : 'a matrix -> bool
val transpose : 'a matrix -> unit (* mutable - modifies matrix in place *)

(* Grab parts of a matrix *)
val getIndex : 'a matrix -> (int * int) -> 'a
val getBand : 'a matrix -> int -> 'a array1d
val getRow : 'a matrix -> int -> 'a array1d
val getColumn : 'a matrix -> int -> 'a array1d
val getElements : 'a matrix -> 'a list

(* change matrix *)
val clone : 'a matrix -> 'a matrix
val setIndex : 'a matrix -> (int * int) -> 'a -> unit
val modify : ('a -> 'a) -> 'a matrix -> unit
val modifyi : (int * int * 'a -> 'a) -> 'a matrix -> unit
val mapi : (int * int * 'a -> 'b) -> 'a matrix -> 'b list

(* Conversion functions *)
val toRows : 'a matrix -> 'a Array.array list
val fromRows : 'a Calculus.calculus -> 'a Array.array list -> 'a matrix
val toColumns : 'a matrix -> 'a Array.array list
val toBands : 'a matrix -> 'a Array.array list
val toPaddedBands : 'a matrix -> 'a Array.array list

(* Normalize methods *)
val normalize : 'a matrix -> unit
val optimize : 'a matrix -> unit

(* Helper functions *)
val toString : 'a matrix -> string list
val infoString : 'a matrix -> string
val print : 'a matrix -> unit
val findBandwidth : 'a matrix -> (int * int)


end

structure Matrix:MATRIX =
struct

type 'a array2d = 'a Array2.array
type 'a array1d = 'a Array.array

datatype 'a matrixtype = BANDED of {ncols:int,
				    nrows:int,
				    upperbw:int,
				    lowerbw:int,
				    calculus: 'a Calculus.calculus,
				    data: 'a array1d list}
		       | DENSE of {calculus: 'a Calculus.calculus,
				   data: ('a array2d)}
type 'a matrix = 'a matrixtype ref

val i2s = Util.i2s

(* Wrapper functions for internal array commands *)
fun Arraysub (arr, i) =
    let
	val length = Array.length arr
    in
	if i < 0 then
	    DynException.stdException(("Index into array ["^(i2s i)^"] can't be less than zero"), "Matrix.Arraysub", Logger.INTERNAL)
	else if i > length then
	    DynException.stdException(("Index into array ["^(i2s i)^"] can't be greater than the length " ^ (i2s length)), "Matrix.Arraysub", Logger.INTERNAL)
	else 
	    Array.sub (arr, i)	    
    end
    handle e => DynException.checkpoint "Matrix.Arraysub" e


(* Utility functions for converting between vectors and arrays *)
fun arrayToList array = Array.foldr op:: nil array
fun vectorToList vector = Vector.foldr op:: nil vector

fun vectorToArray vector = 
    Array.tabulate (Vector.length vector, fn i => Vector.sub (vector, i))


fun TwoDArray2list m = 
    let
	fun TwoDArray2rows m = 
	     map 
		 (fn(i)=>vectorToArray (Array2.row (m, i)))
		 (List.tabulate (Array2.nRows m, (fn(x)=>x)))
    in
	Util.flatmap 
	    arrayToList
	    (TwoDArray2rows m)
    end


fun allzero isZero a = 
(*    let
	fun isZero (Exp.TERM t) = Term.isZero t
	  | isZero _ = false
    in*)
	Array.foldl (fn(item, zero)=>zero andalso isZero item) true a
  (* end *)

fun size m = 
    case !m of
	DENSE {data,...} => Array2.dimensions data
      | BANDED {nrows, ncols,...} => (nrows, ncols)

fun matrix2calculus m =
    case !m of
	DENSE {calculus,...} => calculus
      | BANDED {calculus,...} => calculus

fun isSquare m =
    let
	val (rows, cols) = size m
    in
	rows = cols
    end

fun getIndex m (i, j) = 
    let
	val (rows, cols) = size m

	(* bounds checking *)
	val _ = if i < 0 orelse j < 0 orelse i >= rows orelse j >= cols then
		    DynException.stdException("Invalid index into matrix", "Matrix.getIndex", Logger.INTERNAL)
		else
		    ()
    in
	case !m of
	    DENSE {data=m',...} => Array2.sub (m', i, j)
	  | BANDED {data,calculus,upperbw,lowerbw,...} => 
	    let
		val {zero,...} = calculus
		val ondiag = i=j
		val inlower = i-j > 0 andalso i-j <= lowerbw
		val inupper = j-i > 0 andalso j-i <= upperbw
		val inband = ondiag orelse inlower orelse inupper
	    in
		if inband then
		    let
			val band = i-j+lowerbw
			val pos = if inlower then j else i
		    in
			Arraysub (StdFun.nth (data, band), pos)
		    end
		else
		    zero
	    end
    end
    handle e => DynException.checkpoint ("Matrix.getIndex ["^(i2s i)^","^(i2s j)^"]") e


fun getRow m i =
    let
	val (rows, cols) = size m

	(* bounds checking *)
	val _ = if i < 0 orelse i >= rows then
		    DynException.stdException("Invalid row into matrix", "Matrix.getRow", Logger.INTERNAL)
		else
		    ()
    in
	case !m of
	    DENSE {data=m',...} => vectorToArray (Array2.row (m', i))
	  | BANDED _ => Array.fromList
			    (map
				 (fn(j)=>getIndex m (i, j))
				 (List.tabulate (cols, fn(x)=>x)))
    end
    handle e => DynException.checkpoint ("Matrix.getRow ["^(i2s i)^"]") e

fun getColumn m j =
    let
	val (rows, cols) = size m

	(* bounds checking *)
	val _ = if j < 0 orelse j >= cols then
		    DynException.stdException("Invalid column into matrix", "Matrix.getColumn", Logger.INTERNAL)
		else
		    ()
    in
	case !m of
	    DENSE {data=m',...} => vectorToArray (Array2.column (m', j))
	  | BANDED _ => Array.fromList
			    (map
				 (fn(i)=>getIndex m (i, j))
				 (List.tabulate (rows, fn(x)=>x)))
    end
    handle e => DynException.checkpoint ("Matrix.getColumn ["^(i2s j)^"]") e

fun getElements m = 
    case !m of
	DENSE {data,...} => TwoDArray2list data 
      | BANDED {data,...} => Util.flatmap arrayToList data

(* Pull out the rows from the matrix *)
fun toRows m = 
    let
	val (rows, cols) = size m
    in
	map
	    (fn(i)=>getRow m i)
	    (List.tabulate (rows, (fn(x)=>x)))
    end

(* toString - convert the matrix into an array of strings *)
fun toString m = 
    let
	(* pull out info *)
	val toStrFnc = #toString (matrix2calculus m)
	val (nrows, ncols) = size m
	val rows = toRows m
		   
	(* convert all rows to strings *)
	val elements_as_strings = 
	    map 
		(fn(a)=>map toStrFnc (arrayToList a))
		rows

	(* compute the maximum string sizes *)
	fun extractColumn j = 
	    List.tabulate (nrows, fn(i)=> Util.nth (Util.nth (elements_as_strings, i), j))	    
	    
	fun columnToSize j = 
	    foldl
		(fn(element,max_size)=> 
		   let
		       val element_size = String.size element
		   in
		       if element_size > max_size then
			   element_size
		       else
			   max_size
		   end)
		0
		(extractColumn j)
	    
	val sizes = 
	    map
		columnToSize
		(List.tabulate (ncols, fn(x)=>x))

	(* add padding to each string *)
	fun spaces count = 
	    String.concat (List.tabulate (count, fn(x)=>" "))
	fun padString (str, len) = 
	    let
		val size = String.size str
		val before_count = Real.floor (Real.fromInt (len - size) / 2.0)
		val after_count = len - size - before_count
	    in
		(spaces before_count) ^ str ^ (spaces after_count)
	    end
	val padded_strings = 
	    map
		(fn(row)=> 
		   map
		       padString
		       (ListPair.zip (row, sizes)))
		elements_as_strings

	(* turn each row into a string *)
	val row_strings = map
			      (fn(row)=> "[ " ^  (String.concatWith "  " row) ^ " ]")
			      padded_strings
    in
	row_strings
    end

fun infoString m =
    let
	val (nrows, ncols) = size m
    in
	case !m of 
	    DENSE _ => "DENSE Matrix ["^(i2s nrows)^","^(i2s ncols)^"]"
	  | BANDED {upperbw, lowerbw, ...} => "BANDED Matrix ["^(i2s nrows)^","^(i2s ncols)^"] {upperbw="^(i2s upperbw)^",lowerbw="^(i2s lowerbw)^"}"
    end

(* print - display the matrix on the screen *)
fun print m =
    let
	val str_list = toString m
    in
	(Util.log (infoString m);
	 (app Util.log str_list))
    end

(* convert the rows back to an array2 *)
fun rows2array2 vectors = 
    let
	val lists = map arrayToList vectors
    in
	Array2.fromList lists
    end

(* convert a list of rows back to a matrix *)
fun fromRows calculus rows = 
    let 
	val a = rows2array2 rows
    in
	ref (DENSE {calculus=calculus, data=a})
    end

(* Pull out the columns from the matrix *)
fun toColumns m = 
    let
	val (rows, cols) = size m
    in
	map
	    (fn(j)=>getColumn m j)
	    (List.tabulate (cols, (fn(x)=>x)))	
    end

(* getBand will pull out a particular diagonal band from the matrix. 
  When num is zero, the main diagonal is pulled.  When num is greater than zero, bands
  are taken from the upper triangular matrix.  When num is less than zero, bands are
  taken from the lower triangular matrix. *)
fun getBand m num =
    let
	(*val _ = Util.log ("Calling getBand for band #"^(i2s num)^" on m=" ^ ((e2s o Container.matrix2expmatrix) m))*)
	val (rows, cols) = size m
	val max_dim = if rows > cols then rows else cols
	val _ = if Int.abs num + 1 > rows orelse Int.abs num + 1 > cols then
		    DynException.stdException("Trying to grab band #"^(i2s num)^" that is outside of the matrix's dimensions",
					      "Matrix.getBand",
					      Logger.INTERNAL)
		else
		    ()
    in
	case !m of
	    DENSE {data=m',...} => 
	    let
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
				(fn(i,j)=>getIndex m (i, j))
				valid_indices
	    in
		Array.fromList items
	    end
	  | BANDED {data,upperbw,lowerbw,calculus,...} => 
	    let
		val {zero,...} = calculus
		val array_len = max_dim - (Int.abs num)
		val in_band = num >= (~lowerbw) andalso num <= upperbw				  
	    in
		if in_band then
		    StdFun.nth (data, num+lowerbw)
		else
		    Array.tabulate (array_len, fn(x)=>zero)
	    end
	(*val _ = Util.log (" -> band=" ^ ((e2s o Container.array2exparray) band))*)
    end
    handle e => DynException.checkpoint ("Matrix.getBand ["^(i2s num)^"]") e


(* findBandwidth will determine the number of upper and lower non-zero bands that encompass the matrix *)
fun findBandwidth m = 
    case !m of 
	DENSE {data=m',calculus} => 
	if isSquare m then
	    let
		(*val _ = Util.log ("findBandwidth m=" ^ (e2s (Container.matrix2expmatrix m)))*)
		val (dim, _) = size m
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
						       val {isZero,...} = calculus
						   in
						       if allzero isZero a then
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
						       val {isZero,...} = calculus
						   in
						       if allzero isZero a then
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
	    DynException.stdException("Only supports square matrices", "Matrix.findBandwidth", Logger.INTERNAL)
      | BANDED {upperbw, lowerbw, ...} => (upperbw, lowerbw)

(* pull out the bands from a matrix *)
fun toBands m =
    let
	val (upper_bw, lower_bw) = findBandwidth m
	val band_indices = List.tabulate (upper_bw + lower_bw + 1, fn(x)=> x-lower_bw)
    in
	map 
	    (fn(d)=>getBand m d)
	    band_indices
    end

(* pull out the bands, put add padding such that each padding is the same length *)
fun toPaddedBands m = 
    let
	val (upper_bw, lower_bw) = findBandwidth m
	val band_indices = List.tabulate (upper_bw + lower_bw + 1, fn(x)=> x-lower_bw)
	val bands = toBands m
	val bw = if upper_bw > lower_bw then upper_bw else lower_bw
	val {zero,...} = matrix2calculus m

	fun prependZerosArray num_zeros a = 
	    let
		val zeros = List.tabulate (num_zeros, fn(x)=>zero)
		val a' = Array.fromList (zeros @ (arrayToList a))
	    in
		a'
	    end
	    
	fun appendZerosArray num_zeros a = 
	    let
		val zeros = List.tabulate (num_zeros, fn(x)=>zero)
		val a' = Array.fromList (arrayToList a @ zeros)
	    in
		a'
	    end

	(* grab the diagonal *)
	val diag = getBand m 0
		   
	(* grab the upper bands *)
	val upper_bands = List.tabulate (bw, fn(x)=> appendZerosArray (x+1) (getBand m (x+1)))
			  
	(* grab the lower bands *)
	val lower_bands = List.tabulate (bw, fn(x)=> prependZerosArray (bw-x) (getBand m (~(bw-x))))
    in
	(lower_bands @ [diag] @ upper_bands)
    end

(* clone matrix - makes a copy of a matrix *)
fun clone m =
    case !m of
	DENSE {data,calculus} =>
	let
	    val rows = toRows m
	    val datalist = map arrayToList rows
	    val data' = Array2.fromList datalist
	in
	    ref (DENSE {data=data',calculus=calculus})
	end
      | BANDED {data,calculus,nrows,ncols,upperbw,lowerbw} =>
	let
	    val data' = map (Array.fromList o arrayToList) data
	in
	    ref (BANDED {data=data',calculus=calculus,nrows=nrows,ncols=ncols,upperbw=upperbw,lowerbw=lowerbw})
	end

(* normalize - converts any matrix type into a dense matrix *)
fun normalize m = 
     case !m of
	 DENSE _ => ()
       | BANDED {data,nrows,ncols,upperbw,lowerbw,calculus} =>
	 let
	     val rows = toRows m
	     val data = rows2array2 rows
	 in
	     m := (DENSE {data=data, calculus=calculus})
	 end

(* optimize - try to find the best internal representation for the given matrix *)
fun optimize m =
    let
	val m' = clone m
	val _ = normalize m'
	val (nrows, ncols) = size m'
    in
	if isSquare m then
	    let
		val dim = nrows

		(* check the bandwidth of the matrix, a banded matrix might be better *)
		val (upperbw, lowerbw) = findBandwidth m'
	    in			 
		if upperbw + lowerbw + 1 < dim then
		    let
			val bands = map (getBand m') (List.tabulate (upperbw+lowerbw+1, fn(i)=>i-lowerbw))
			(*val _ = Util.log ("Displaying bands")*)
			val toStrFcn = #toString (matrix2calculus m)
			val strs = map 
				       (fn(b)=>String.concatWith ", " (map toStrFcn (arrayToList b)))
				       bands
			(*val _ = app
				    (fn(str)=>Util.log ("{"^(str)^"}"))
				    strs*)
		    in
			m := (BANDED {nrows=nrows,
				      ncols=ncols,
				      upperbw=upperbw,
				      lowerbw=lowerbw,
				      calculus=matrix2calculus m',
				      data=bands})
		    end
		else
		    () (* keep what we had *)
	    end
	else
	    () (* we can't simplify non-square matrices right now *)
    end

(* set the value at the index *)
fun setIndex m (i, j) value = 
    let
	val (rows, cols) = size m

	(* bounds checking *)
	val _ = if i < 0 orelse j < 0 orelse i >= rows orelse j >= cols then
		    DynException.stdException("Invalid index into matrix", "Matrix.setIndex", Logger.INTERNAL)
		else
		    ()
    in
	case !m of 
	    DENSE {data=m',...} => Array2.update (m', i, j, value)
	  | BANDED {data,calculus,upperbw,lowerbw,...} => 
	    let
		val {zero,...} = calculus
		val ondiag = i=j
		val inlower = i-j <= lowerbw
		val inupper = j-i <= upperbw
		val inband = inlower orelse inupper
	    in
		if inband then
		    let
			val band = i-j+lowerbw
			val pos = i
		    in
			Array.update (StdFun.nth (data, band), pos, value)
		    end
		else
		    setIndex (normalize m;m) (i, j) value
	    end	    
    end
    handle e => DynException.checkpoint ("Matrix.setIndex ["^(i2s i)^","^(i2s j)^"]") e

fun zeros calculus (rows, cols) = 
    ref (DENSE {calculus=calculus, 
		data=Array2.array (rows, cols, #zero calculus)})

fun identity calculus dim = 
    ref (BANDED {calculus=calculus,
		 nrows=dim,
		 ncols=dim,
		 upperbw=0,
		 lowerbw=0,
		 data=[Array.tabulate (dim, (fn(x)=> #one calculus))]})

fun transpose m =
    case !m of
	BANDED _ => transpose (normalize m;m)
      | DENSE {data, calculus} => 
	let
	    val (nrows, ncols) = size m
	    val data' = Array2.array (ncols, nrows, #zero calculus)
	    val data_region = {base=data, row=0, col=0, nrows=NONE, ncols=NONE}
	    val _ = Array2.appi Array2.RowMajor (fn(i,j,e)=>Array2.update (data', j, i, e)) data_region
	in
	    m := (DENSE {data=data',
			 calculus=calculus})
	end

(* modify - modify function over each of the elements of the matrix *)
fun modify modifyfun m = 
    case !m of
	BANDED {data, calculus={zero,isZero,...},...} =>
	(* first test the zero, if it changes when applied to appfun, the matrix must be evaluated as a dense matrix *)
	if isZero (modifyfun zero) then
	    app (fn(a)=> Array.modify modifyfun a) data
	else
	    modify modifyfun (normalize m;m)
      | DENSE {data,calculus} =>
	Array2.modify Array2.RowMajor modifyfun data

(* modifyi - modify function over each of the elements of the matrix, include a row/col index *)
fun modifyi modifyfun m = 
    case !m of
	BANDED {data,calculus={zero,isZero,...},...} =>
	(* we can't really test each of the zero elements differently, so we have to convert to a normal matrix first *)
	modifyi modifyfun (normalize m;m)
      | DENSE {data,calculus} => 
	Array2.modifyi Array2.RowMajor modifyfun {base=data, col=0, row=0, ncols=NONE, nrows=NONE}

(* run a map function across each element of the matrix, passing along the index *)
fun mapi mapfun m = 
    let
	val rows = toRows m
    in
	Util.flatmap 
	    (fn(r,i)=> map (fn(e,j)=> mapfun (i, j, e)) (StdFun.addCount (arrayToList r)))
	    (StdFun.addCount rows)
    end

(* (\* Perform the JSON operations *\) *)
(* local open mlJS in *)

(* fun array_to_json to_json a = *)
(*     js_array (map to_json (arrayToList a))     *)

(* fun to_json m = *)
(*     case !m of *)
(* 	DENSE {data,calculus} =>  *)
(* 	let *)
(* 	    val {toJSON=to_json,...} = calculus *)
(* 	    val (nrows, ncols) = size m *)
(* 	in *)
(* 	    js_object [("type", js_string "MATRIX"), *)
(* 		       ("subtype", js_string "DENSE"), *)
(* 		       ("rows", js_int nrows), *)
(* 		       ("columns", js_int ncols), *)
(* 		       ("members", js_array (map  *)
(* 						 (array_to_json to_json) *)
(* 						 (toRows m)))] *)
(* 	end *)
(*       | BANDED {nrows,ncols,upperbw,lowerbw,data,calculus} => *)
(* 	let *)
(* 	    val {toJSON=to_json,...} = calculus *)
(* 	in *)
(* 	    js_object [("type", js_string "MATRIX"), *)
(* 		       ("subtype", js_string "BANDED"), *)
(* 		       ("rows", js_int nrows), *)
(* 		       ("columns", js_int ncols), *)
(* 		       ("upperbw", js_int upperbw), *)
(* 		       ("lowerbw", js_int lowerbw), *)
(* 		       ("members", js_array (map  *)
(* 						 (array_to_json to_json) *)
(* 						 (toBands m)))] *)
(* 	end *)

(* end *)

end
