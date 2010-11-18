signature SUBSPACE =
sig

    (* An interval defines how to select across one dimension of data.  The top four types, Empty, Full,
     * Interval, and Indices can be used to iterate across one dimension of a Tensor or other geometric 
     * space.  The IntervalCollection defines how to iterator across a collection of spaces. *)
    datatype interval = 
	     (* An Empty interval has no values included *)
	     Empty
	   (* A Full interval includes every value - it's the same as an interval going from 0 to n-1, step 1. *)
	   | Full
	   (* An Interval is a range from start to stop by step.  It is valid if step < 0 but not if
	    * step=0.  The usage follows the Matlab convention of start:step:stop, except that everything 
	    * is zero indexed. *)
	   | Interval of {start: int, stop: int, step: int} (* this is all zero indexed *)
	   (* Indices are a lit of one or more values in any arbitray order.  Singluar indices are encoded as 
	    * a one element list. *)
	   | Indices of int list
	   (* When you are subreferencing into a collection of multiple spaces (see space.sml), 
	    * you need an IntervalCollection to index into it.  The first interval in the tuple 
	    * refers to which elements of the collection you want to reference.  The second tupe
	    * value, the subspace list, has the same length as the number of elements chosen out 
	    * of the space collection.  Those intervals are then applied recursively to each space
	    * in the collection.*)
	   | IntervalCollection of (interval * interval list list)

    (* the subspace is a list of intervals, with the assumption is that the length of the interval 
     * list is equivalent to the number of dimensions *)
    type subspace = interval list

    (* ------ Methods on SubSpaces ------*)
    val toString : subspace -> string        (* create a string representation for logging *)
    val toLayout : subspace -> Layout.t   (* create a string representation for logging *)
    val toJSON : subspace -> JSON.json    (* create the serialized json representation *)
				     
end
structure SubSpace : SUBSPACE =
struct

(* define a subspace as a listing of intervals *)
datatype interval = Empty
		  | Full
		  | Interval of {start: int, stop: int, step: int} (* this is all zero indexed *)
		  | Indices of int list
		  | IntervalCollection of (interval * interval list list)
type subspace = interval list (* assumption is that the length of the interval list is equivalent
			       * to the number of dimensions *)


fun toString subspace =
    let
	val i2s = Util.i2s
	fun intervalToStr Empty = "_"
	  | intervalToStr Full = ":"
	  | intervalToStr (Interval {start,step,stop}) = 
	    if step = 1 then
		(i2s start) ^ ":" ^ (i2s stop)
	    else
		(i2s start) ^ ":" ^ (i2s step) ^ ":" ^ (i2s stop)
	  | intervalToStr (Indices [int]) = i2s int
	  | intervalToStr (Indices int_list) = "[" ^ (String.concatWith ", " (map i2s int_list)) ^ "]" 
	  | intervalToStr (IntervalCollection (interval, subspace_list)) = "("^(intervalToStr interval)^", {"^ (String.concatWith ", " (map toString subspace_list)) ^"})"

	val interval_str = String.concatWith ", " (map intervalToStr subspace)
    in
	"[" ^ interval_str ^ "]"
    end

fun toLayout subspace =
    let
	open Layout
	val int = str o Util.i2s
	fun intervalToLayout Empty = str "_"
	  | intervalToLayout Full = str ":"
	  | intervalToLayout (Interval {start,step,stop}) = 
	    if step = 1 then
		seq[int start, str ":", int stop]
	    else
		seq[int start, str ":", int step, str ":", int stop]
	  | intervalToLayout (Indices int_list) = bracketList (map int int_list)
	  | intervalToLayout (IntervalCollection (interval, subspace_list)) = 
	    parenList [intervalToLayout interval,
		       curlyList (map toLayout subspace_list)]

    in
	bracketList (map intervalToLayout subspace)
    end


fun toJSON subspace =
    let
	open JSON
	open JSONExtensions
	fun intervalToJSON Empty = JSONType "Empty"
	  | intervalToJSON Full = JSONType "Full"
	  | intervalToJSON (Interval {start, step, stop}) = JSONTypedObject ("Interval", array [int (IntInf.fromInt start), 
												int (IntInf.fromInt step),
												int (IntInf.fromInt stop)])
	  | intervalToJSON (Indices int_list) = JSONTypedObject ("Indices", 
								 array (map (int o IntInf.fromInt) int_list))
	  | intervalToJSON (IntervalCollection (interval, subspace_list)) = 
	    JSONTypedObject ("IntervalCollection", object [("interval", intervalToJSON interval),
							   ("subspaces", array (map toJSON subspace_list))])
    in
	array (map intervalToJSON subspace)
    end




end
