signature SUBSPACE =
sig

    (* define a subspace as a listing of intervals *)
    datatype interval = Empty
		      | Full
		      | Interval of {start: int, stop: int, step: int} (* this is all zero indexed *)
		      | Indices of int list
		      | IntervalCollection of (interval * interval list list)
    type subspace = interval list (* assumption is that the length of the interval list is equivalent
				   * to the number of dimensions *)

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
