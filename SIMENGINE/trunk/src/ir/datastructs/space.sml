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
    val isScalar : space -> bool             (* test to see if it's a scalar space *)
    val isEmpty : space -> bool              (* test to see if it's an empty space *)
    val collection : space list -> space     (* combine spaces into a collection of spaces *)
    val isCollection: space -> bool          (* test for collections - if it's a collection, we can't operate over it, just subreference it *)
    val tensor : int list -> space           (* construct a tensor space *)
    val equal : (space * space) -> bool      (* see if two spaces are equal *)

    (* -------- Methods for Math --------*)
    val reduceCodomain : space -> space      (* replace the first dimension quantity from a space with a singleton *)
    val isMatrix : space -> bool
    val isVector : space -> bool
    val toMatrixDims : space -> (int * int)
    val toVectorDim : space -> int
    val fromMatrixDims : (int * int) -> space
    val fromVectorDim : int -> space

    (* ------ Methods on SubSpaces ------*)
    val subspaceToStr : subspace -> string        (* create a string representation for logging *)
    val subspaceToLayout : subspace -> Layout.t   (* create a string representation for logging *)
    val subspaceToJSON : subspace -> JSON.json    (* create the serialized json representation *)

end
structure Space: SPACE =
struct

(* there are four types of spaces *)
datatype space =
	 Point of pointspace
       | Collection of space list
       (* comment these out until we are ready *)
       (*| Curve
       | Surface
       | Volume*)

(* for a point space, there is a tensor or one based on a coordinate system *)
and pointspace =
    Tensor of int list (* the int list is the dimensions *)
  (* comment these out until we are ready *)
  (*| Rectangular of {l: range, w: range, h: range}
  | Cylindrical of {rho: range, phi: range, z: range}
  | Spherical of {r: range, theta: range, phi: range}*)

(* define a range as a length in the dimension of the point space *)
withtype range = {start: real, stop: real, steps: real}


(* define a subspace as a listing of intervals *)
datatype interval = Empty
		  | Full
		  | Interval of (int * int) (* this is all zero indexed *)
		  | Indices of int list
		  | IntervalCollection of (interval * interval list list)
type subspace = interval list (* assumption is that the length of the interval list is equivalent
			       * to the number of dimensions *)

fun except msg = DynException.stdException (msg, "Space", Logger.INTERNAL)
fun error msg = (Logger.log_error (Printer.$ (msg)); DynException.setErrored())

val empty_space = Point (Tensor [])

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
	      | pairToDim (dim, IntervalCollection _) =
		(error "interval collections are not supported inside tensor spaces"; 0)
	    val dims = map pairToDim pairs
	    val dims' = List.filter (fn(d)=> d>0) dims
	in
	    Point (Tensor dims')
	end
    else
	except "Unexpected dimension mismatch"
  | sub (space as (Collection spaces)) subspace =
	 let val dim = length spaces
	 in case (spaces, subspace) of
		([], []) => empty_space
	      | ([], _) => (error "can't subreference from an empty collection"; 
			    empty_space)
	      | (_, []) => (error "index must have at least one dimension";
			    space)
	      | (_, [IntervalCollection (interval, subspace_list)]) => 
		let
		    fun recurse spaces' =
			let
			    val pairs = ListPair.zip (spaces', subspace_list)
			in
			    Collection (map (fn(space', subspace')=>  sub space' subspace') pairs)		 
			end
			    
		    fun filterSpaces indices =
			let
			    val spaces' = map (fn(index)=> List.nth (spaces, index+1)) indices
			in
			    recurse spaces'
			end
		in
		(case interval of
		     Empty => empty_space
		   | Interval (a, b) => 
		     let
			 val indices = 
			     if a < 0 then
				 (error "index must by 0 or greater"; [])
			     else if b >= dim then
				 (error "index must be less than the dimension"; [])
			     else if b < a then
				 (error "second index must be greater than or equal to the first index"; [])
			     else
				 List.tabulate (b - a + 1, fn(x)=>x+a)
		     in
			 filterSpaces indices
		     end
		   | Indices indices => 
		     let
			 val indices = 
			     if List.exists (fn(a)=>a < 0) indices then
				 (error "indices must by 0 or greater"; [])
			     else if List.exists (fn(b) => b >= dim) indices then
				 (error "indices must be less than dimension"; [])
			     else
				 indices
		     in
			 filterSpaces indices
		     end
		   | Full => 
		     if length subspace_list = dim then
			 recurse spaces
		     else
			 (error "when using full indexing, the interval list must be equal in size to the collection";
			  space)
		   | IntervalCollection _ => (error "unexpected interval collection"; space))
		end
	      | (_, _) => (error "unexpected non interval collection indexing found in collection space";
			 space)
	 end
	 
fun empty (Point (Tensor dims)) = []
  | empty (Collection spaces) = [IntervalCollection (Empty, [])]
fun full (Point (Tensor dims)) = map (fn(_)=>Full) dims
  | full (Collection spaces) = [IntervalCollection (Full, map full spaces)]
fun size (Point (Tensor dims)) = Util.prod dims
  | size (Collection spaces) = Util.sum (map size spaces)
val scalar = Point (Tensor [1])
fun tensor dims = Point (Tensor dims) 
fun isEmpty (Point (Tensor [])) = true
  | isEmpty (Collection []) = true
  | isEmpty (Collection [Point (Tensor [])]) = true
  | isEmpty _ = false
fun isScalar (Point (Tensor [1])) = true
  | isScalar _ = false
fun collection spaces = Collection spaces
fun isCollection (Collection _) = true
  | isCollection _ = false

fun reduceCodomain (Point (Tensor [])) = scalar (* special case which causes the default value to emerge *)
  | reduceCodomain (Point (Tensor [dim])) = scalar
  | reduceCodomain (Point (Tensor (dim::dims))) = Point (Tensor (1::dims))
  | reduceCodomain (Collection _) = except "Can not perform a codomain reduction on a space collection"

fun isMatrix (Point (Tensor [x, y])) = true
  | isMatrix (Point _) = false
  | isMatrix (Collection _) = false

fun toMatrixDims (Point (Tensor [x, y])) = (x,y)
  | toMatrixDims _ = except "unexpected non matrix"
fun fromMatrixDims (x, y) = tensor [x,y]

fun isVector (Point (Tensor [x])) = true
  | isVector (Point _) = false
  | isVector (Collection _) = false

fun toVectorDim (Point (Tensor [x])) = x
  | toVectorDim _ = except "unexpected non vector"
fun fromVectorDim x = tensor [x]

fun equal_pointspace (pointspace1, pointspace2) = 
    case (pointspace1, pointspace2) of
	(Tensor dim1, Tensor dim2) => (length dim1) = (length dim2) andalso
				      List.all (fn(d1,d2)=>d1=d2) (ListPair.zip (dim1, dim2))

fun equal (space1, space2) = 
    case (space1, space2) of
	(Point pointspace1, Point pointspace2) => equal_pointspace (pointspace1, pointspace2)
      | (Collection [Point pointspace1], Point pointspace2) => equal_pointspace (pointspace1, pointspace2)
      | (Point pointspace1, Collection [Point pointspace2]) => equal_pointspace (pointspace1, pointspace2)
      | (Collection spaces1, Collection spaces2) => (length spaces1) = (length spaces2) andalso
						    List.all equal (ListPair.zip (spaces1, spaces2))
      | _ => false

fun subspaceToStr subspace =
    let
	val i2s = Util.i2s
	fun intervalToStr Empty = "_"
	  | intervalToStr Full = ":"
	  | intervalToStr (Interval (a,b)) = (i2s a) ^ ":" ^ (i2s b)
	  | intervalToStr (Indices int_list) = "[" ^ (String.concatWith ", " (map i2s int_list)) ^ "]" 
	  | intervalToStr (IntervalCollection (interval, subspace_list)) = "("^(intervalToStr interval)^", {"^ (String.concatWith ", " (map subspaceToStr subspace_list)) ^"})"

	val interval_str = String.concatWith ", " (map intervalToStr subspace)
    in
	"[" ^ interval_str ^ "]"
    end

fun subspaceToLayout subspace =
    let
	open Layout
	val int = str o Util.i2s
	fun intervalToLayout Empty = str "_"
	  | intervalToLayout Full = str ":"
	  | intervalToLayout (Interval (a,b)) = seq[int a, str ":", int b]
	  | intervalToLayout (Indices int_list) = bracketList (map int int_list)
	  | intervalToLayout (IntervalCollection (interval, subspace_list)) = 
	    parenList [intervalToLayout interval,
		       curlyList (map subspaceToLayout subspace_list)]

    in
	bracketList (map intervalToLayout subspace)
    end

fun subspaceToJSON subspace =
    let
	open JSON
	open JSONExtensions
	fun intervalToJSON Empty = JSONType "Empty"
	  | intervalToJSON Full = JSONType "Full"
	  | intervalToJSON (Interval (a, b)) = JSONTypedObject ("Interval", array [int (IntInf.fromInt a), 
										   int (IntInf.fromInt b)])
	  | intervalToJSON (Indices int_list) = JSONTypedObject ("Indices", 
								 array (map (int o IntInf.fromInt) int_list))
	  | intervalToJSON (IntervalCollection (interval, subspace_list)) = 
	    JSONTypedObject ("IntervalCollection", object [("interval", intervalToJSON interval),
							   ("subspaces", array (map subspaceToJSON subspace_list))])
    in
	array (map intervalToJSON subspace)
    end


end
