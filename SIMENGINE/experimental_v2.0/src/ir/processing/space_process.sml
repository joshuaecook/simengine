signature SPACEPROCESS = 
sig


    (* ------ Methods on Spaces ------*)
    val sub : Space.space -> Exp.subspace -> Space.space  (* create a new space out of a subspace of an existing space *)
    val empty : Space.space -> Exp.subspace               (* create an empty subspace from an existing space *)
    val full : Space.space -> Exp.subspace                (* create a subspace encompassing the entire space *)

    (* ----- Inverted Function -------*)
    val setExpToSpace : (Exp.exp -> Space.space) -> unit  (* define a method that can determine the space from an Exp *)
    val expToSpace : (Exp.exp -> Space.space)             (* use the method just as it is defined in ExpSpace *)

end
structure SpaceProcess : SPACEPROCESS =
struct

open Space (* for space datatype *)
open Exp   (* for subspace datatype *)

fun except msg = DynException.stdException (msg, "SpaceProcess", Logger.INTERNAL)
fun error msg = (Logger.log_error (Printer.$ (msg)); DynException.setErrored())

val expToSpaceRef = ref (fn(exp)=>Space.emptyCollection)
fun setExpToSpace fcn = (expToSpaceRef := fcn)
fun expToSpace exp = !expToSpaceRef exp

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

fun empty (Point (Tensor dims)) = []
  | empty (Collection spaces) = [IntervalCollection (Empty, [])]
fun full (Point (Tensor dims)) = map (fn(_)=>Full) dims
  | full (Collection spaces) = [IntervalCollection (Full, map full spaces)]

(* given a space, come up with a way to convert that to a subspace that explicitly covers each element *)
fun spaceToSubSpace (Point (Tensor dims)) =
    map (fn(d)=> Interval {start=0, step=1, stop=d}) dims
  | spaceToSubSpace (Collection spaces) =
    [IntervalCollection (Interval {start=0, step=1, stop=List.length spaces},
			 map spaceToSubSpace spaces)]


fun sub (space as (Point (Tensor dims))) subspace =
    if (length dims) = (length subspace) then
	let
	    (* redefine because sub errors will cause SpaceExceptions in expToSpace *)
	    val error = except
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
	      | pairToDim (dim, NamedInterval (_, i)) = pairToDim (dim, i)
	      | pairToDim (dim, ExpInterval exp) =
		let val space' = expToSpace exp
		in case spaceToSubSpace space' of
		       [] => 0
		     | [interval] => pairToDim (dim, interval)
		     | _ => (error "expression intervals should be only one dimension"; 0)
		end
	    val dims = map pairToDim pairs
	    val dims' = List.filter (fn(d)=> d>0) dims
	in
	    Point (Tensor dims')
	end
    else
	except "Unexpected dimension mismatch"
  | sub (space as (Collection spaces)) subspace =
    let val dim = length spaces
	(* redefine because sub errors will cause SpaceExceptions in expToSpace *)
	val error = except
    in case (spaces, subspace) of
	   ([], []) => emptyCollection
	 | ([], _) => (error "can't subreference from an empty collection"; 
		       emptyCollection)
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

	       fun interval_to_space interal = 
		   case interval of
		       Empty => emptyCollection
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
		     | IntervalCollection _ => (error "unexpected interval collection"; space)
		     | NamedInterval (_, interval) => interval_to_space interval
		     | ExpInterval exp => 
		       let val space' = expToSpace exp
		       in 
			   case spaceToSubSpace space' of
			       [] => emptyCollection
			     | [interval] => interval_to_space interval
			     | _ => (error "Expression intervals can only be single dimension";
				     space')
		       end
	   in
	       interval_to_space interval
	   end
	 | (_, _) => (error "unexpected non interval collection indexing found in collection space";
		      space)
    end


end
