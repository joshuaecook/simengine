structure SubSpace =
struct

(* define a subspace as a listing of intervals *)
datatype interval = Empty
		  | Full
		  | Interval of (int * int) (* this is all zero indexed *)
		  | Indices of int list
		  | IntervalCollection of (interval * interval list list)
type subspace = interval list (* assumption is that the length of the interval list is equivalent
			       * to the number of dimensions *)


end
