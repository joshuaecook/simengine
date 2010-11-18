signature SPACE =
sig

    (* a definition of a space - can be anything from a point space to being multi-dimensional *)
    type space

    (* a portion of a space that is specified during indexing *)
    type subspace = SubSpace.subspace


    (* ------ Methods on Spaces ------*)
    val sub : space -> subspace -> space     (* create a new space out of a subspace of an existing space *)
    val empty : space -> subspace            (* create an empty subspace from an existing space *)
    val full : space -> subspace             (* create a subspace encompassing the entire space *)
    val size : space -> int                  (* count the elements in the space *)
    val scalar : space                       (* construct a scalar space *)
    val isScalar : space -> bool             (* test to see if it's a scalar space *)
    val isEmpty : space -> bool              (* test to see if it's an empty space *)
    val emptyCollection : space              (* return an empty collection *)
    val collection : space list -> space     (* combine spaces into a collection of spaces *)
    val isCollection: space -> bool          (* test for collections - if it's a collection, we can't operate over it, just subreference it *)
    val separate: space -> space list        (* separate a space into a collection of spaces *)
    val multiply: (space * space) -> space   (* cascade multiple spaces together *)
    val tensor : int list -> space           (* construct a tensor space *)
    val equal : (space * space) -> bool      (* see if two spaces are equal *)
    val toLayout : space -> Layout.t         (* convert the space to a layout for debugging *)
    val toString : space -> string           (* convert the space to a string for debugging *)


    (* -------- Methods for Math --------*)
    val reduceCodomain : space -> space      (* replace the first dimension quantity from a space with a singleton *)
    val isMatrix : space -> bool
    val isVector : space -> bool
    val toMatrixDims : space -> (int * int)
    val toVectorDim : space -> int
    val fromMatrixDims : (int * int) -> space
    val fromVectorDim : int -> space


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
open SubSpace

fun except msg = DynException.stdException (msg, "Space", Logger.INTERNAL)
fun error msg = (Logger.log_error (Printer.$ (msg)); DynException.setErrored())

val empty_space = Point (Tensor [])

local
    open Layout
    val int = str o Util.i2s
in
fun toLayout (Point (Tensor dims)) = label ("Tensor", bracketList (map int dims))
  | toLayout (Collection spaces) = label ("Collection", parenList (map toLayout spaces))
end
val toString  = Layout.toString o toLayout



(* methods operating over spaces *)
local
    val i2r = Real.fromInt
    val r2i = Real.floor
in
fun count(start, step, stop) = r2i(i2r (stop-start)/(i2r step))+1
fun count_interval dim {start, step, stop} = 
    if step = 0 then
	(error "step size can not be zero"; 0)
    else if step > 0 then
	if start < 0 then
	    (error "index must be 0 or greater"; 0)
	else if stop >= dim then
	    (error "index must be less than the dimension"; 0)
	else if stop < start then
	    (error "second index must be greater than or equal to the first index"; 0)
	else
	    count (start, step, stop)
    else
	if stop < 0 then
	    (error "index must be 0 or greater"; 0)
	else if start >= dim then
	    (error "index must be less than the dimension"; 0)
	else if start < stop then
	    (error "first index must be greater than or equal to the second index when the step < 0"; 0)
	else
	    count (start, step, stop)		    
end
fun sub (space as (Point (Tensor dims))) subspace =
    if (length dims) = (length subspace) then
	let
	    (*
	    val _ = 
		let
		    val heading = Layout.heading
		    val label = Layout.label
		    val align = Layout.align
		    val l = heading("Space.sub", 
				    align[label("Space", toLayout space),
					  label("SubSpace", subspaceToLayout subspace)])
		in
		    Layout.log l
		end
	     *)
	    val pairs = ListPair.zip (dims, subspace)

	    fun pairToDim (dim, Empty) = 0
	      | pairToDim (dim, Full) = dim
	      | pairToDim (dim, Interval (i as {start, step, stop})) = count_interval dim i
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
		   | Interval (i as {start, step, stop}) => 
		     let
			 val num_interval = count_interval dim i
			 val indices = if num_interval = 0 then
					   []
				       else
					   List.tabulate (num_interval, fn(x) => x*step + start)
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
  | isEmpty (Point (Tensor [0])) = true
  | isEmpty (Collection []) = true
  | isEmpty (Collection [Point (Tensor [])]) = true
  | isEmpty _ = false
fun isScalar (Point (Tensor [1])) = true
  | isScalar (Collection [s]) = isScalar s
  | isScalar _ = false


val emptyCollection  = Collection []
fun collection spaces = Collection spaces
fun isCollection (Collection _) = true
  | isCollection _ = false

fun separate (Collection spaces) = spaces
  | separate space = [space]

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


fun multiply (space1, space2) = 
    if isEmpty space1 then
	emptyCollection
    else if isEmpty space2 then
	emptyCollection
    else if isScalar space1 then
	space2
    else if isScalar space2 then
	space1
    else case (space1, space2) of
	     (Point (Tensor dims1), Point (Tensor dims2)) => tensor (dims1 @ dims2)
	   | _ => (error ("Do not know how to multiply space "^(toString space1)^" with "^(toString space2));
		   space1)



end
