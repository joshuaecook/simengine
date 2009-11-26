signature MATRIX =
sig

(* Matrix operations *)
val isSquare : Container.matrix -> bool
val getBand : Container.matrix -> int -> Container.array
val findBandwidth : Container.matrix -> (int * int)
val dense2bandedmatrix : Container.matrix -> Container.matrix

end

structure Matrix =
struct

val e2s = ExpPrinter.exp2str
val i2s = Util.i2s
val list2array = Container.list2array
val array2list = Container.array2list

fun allzero a =
    let
	fun isZero (Exp.TERM t) = Term.isZero t
	  | isZero _ = false
    in
	Array.foldl (fn(item, zero)=>zero andalso isZero item) true a
    end				     

fun isSquare m =
    let
	val (rows, cols) = Container.matrix2size m
    in
	rows = cols
    end

(* getBand will pull out a particular diagonal band from the matrix. 
  When num is zero, the main diagonal is pulled.  When num is greater than zero, bands
  are taken from the upper triangular matrix.  When num is less than zero, bands are
  taken from the lower triangular matrix. *)
fun getBand m num =
    let
	(*val _ = Util.log ("Calling getBand for band #"^(i2s num)^" on m=" ^ ((e2s o Container.matrix2expmatrix) m))*)
	val (rows, cols) = Container.matrix2size m
	val max_dim = if rows > cols then rows else cols
	val _ = if Int.abs num + 1 > rows orelse Int.abs num + 1 > cols then
		    DynException.stdException("Trying to grab band #"^(i2s num)^" that is outside of the matrix's dimensions",
					      "Matrix.getBand",
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
	val band = list2array items
	(*val _ = Util.log (" -> band=" ^ ((e2s o Container.array2exparray) band))*)
    in
	band
    end

(* findBandwidth will determine the number of upper and lower non-zero bands that encompass the matrix *)
fun findBandwidth m =
    if isSquare m then
	let
	    (*val _ = Util.log ("findBandwidth m=" ^ (e2s (Container.matrix2expmatrix m)))*)
	    val (dim, _) = Container.matrix2size m
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
	DynException.stdException("Only supports square matrices", "Matrix.findBandwidth", Logger.INTERNAL)

(* convert a dense matrix to a banded matrix - this will be a list of arrays *)
fun dense2bandedlist m =
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
	DynException.stdException("Only supports square matrices", "Matrix.dense2bandedlist", Logger.INTERNAL)


fun dense2bandedmatrix m =
    if isSquare m then
	let
	    val (upper_bw,lower_bw) = findBandwidth m
	    val bw = if upper_bw > lower_bw then upper_bw else lower_bw

	    fun prependZerosArray num_zeros a = 
		let
		    val zeros = List.tabulate (num_zeros, fn(x)=>Exp.TERM (Exp.INT 0))
		    val a' = list2array (zeros @ (array2list a))
		in
		    a'
		end

	    (* grab the diagonal *)
	    val diag = getBand m 0
		       
	    (* grab the upper bands *)
	    val upper_bands = List.tabulate (bw, fn(x)=> prependZerosArray (x+1) (getBand m (x+1)))

	    (* grab the lower bands *)
	    val lower_bands = List.tabulate (bw, fn(x)=> prependZerosArray (x+1) (getBand m (~(x+1))))
	in
	    Container.rows2matrix (upper_bands @ [diag] @ lower_bands)
	end
    else
	DynException.stdException("Only supports square matrices", "Matrix.dense2bandedlist", Logger.INTERNAL)


end
