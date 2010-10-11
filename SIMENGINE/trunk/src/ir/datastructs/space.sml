signature SPACE =
sig

    (* a definition of a space - can be anything from a point space to being multi-dimensional *)
    type space

    (* a portion of a space that is specified during indexing *)
    type subspace


    (* ------ Methods on Spaces ------*)
    val sub : space -> subspace -> space     (* create a new space out of a subspace of an existing space *)
    val empty : space -> subspace            (* create an empty subspace from an existing space *)
    val full : space -> subspace             (* create a subspace encompassing the entire space *)
    val size : space -> int                  (* count the elements in the space *)
    val scalar : space                       (* construct a scalar space *)
    val tensor : int list -> space           (* construct a tensor space *)

end
structure Space: SPACE =
struct

(* there are four types of spaces *)
datatype space =
	 Point of pointspace
       | Curve
       | Surface
       | Volume

(* for a point space, there is a tensor or one based on a coordinate system *)
and pointspace =
    Tensor of int list (* the int list is the dimensions *)
  | Rectangular of {l: range, w: range, h: range}
  | Cylindrical of {rho: range, phi: range, z: range}
  | Spherical of {r: range, theta: range, phi: range}

(* define a range as a length in the dimension of the point space *)
withtype range = {start: real, stop: real, steps: real}


(* define a subspace as a listing of intervals *)
datatype interval = Empty
		  | Full
		  | Interval of (int * int) (* this is all zero indexed *)
		  | Indices of int list
type subspace = interval list (* assumption is that the length of the interval list is equivalent
			       * to the number of dimensions *)

fun except msg = DynException.stdException (msg, "Space", Logger.INTERNAL)
fun error msg = (Logger.log_error (Printer.$ (msg)); DynException.setErrored())

(* methods operating over spaces *)
fun sub (Point (Tensor dims)) subspace =
    if (length dims) = (length subspace) then
	let
	    val pairs = ListPair.zip (dims, subspace)
	    fun pairToDim (dim, Empty) = 0
	      | pairToDim (dim, Full) = dim
	      | pairToDim (dim, Interval (a, b)) = 
		if a < 0 then
		    (error "index must by 0 or greater"; 0)
		else if b >= dim then
		    (error "index must be less than the dimension"; 0)
		else if b < a then
		    (error "second index must be greater than or equal to the first index"; 0)
		else
		    b - a + 1
	      | pairToDim (dim, Indices indices) =
		if List.exists (fn(a)=>a < 0) indices then
		    (error "indices must by 0 or greater"; 0)
		else if List.exists (fn(b) => b >= dim) indices then
		    (error "indices must be less than dimension"; 0)
		else
		    length indices
	    val dims = map pairToDim pairs
	    val dims' = List.filter (fn(d)=> d>0) dims
	in
	    Point (Tensor dims')
	end
    else
	except "Unexpected dimension mismatch"

fun empty (Point (Tensor dims)) = []
fun full (Point (Tensor dims)) = map (fn(_)=>Full) dims
fun size (Point (Tensor dims)) = Util.prod dims
val scalar = Point (Tensor [1])
fun tensor dims = Point (Tensor dims) 

end
