signature SPACE =
sig

    (* a definition of a space - can be anything from a point space to being multi-dimensional *)
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


    (* ------ Methods on Spaces ------*)
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
    val toJSON : space -> JSON.json          (* serialize the space to JSON format *)

    (* -------- Methods for Math --------*)
    val reduceCodomain : space -> space      (* replace the first dimension quantity from a space with a singleton *)
    val isTensor : space -> bool
    val isMatrix : space -> bool
    val isVector : space -> bool
    val toDims : space -> int list
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

fun toJSON space =
    let
	open JSON
	open JSONExtensions
    in
	case space of
	    Point p =>
	    JSONTypedObject ("Point", 
			     case p of 
				 Tensor dims => 
				 JSONTypedObject ("Tensor", 
						  array (map (int o IntInf.fromInt) dims)))
	  | Collection spaces =>
	    JSONTypedObject ("Collection",
			     array (map toJSON spaces))
    end



	 
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

fun isTensor (Point (Tensor _)) = true
  | isTensor _ = false

fun toDims (Point (Tensor dims)) = dims
  | toDims _ = except "unexpected non-tensor"

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
