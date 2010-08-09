signature SORTING = sig

(* Returns a new list having its items in ascending order 
 * according to the given ordering function. *)
val sorted: ('a * 'a -> order) -> 'a list -> 'a list

end

structure Sorting: SORTING = struct

fun insertionSort order =
    let fun insert (x, nil) = [x]
	  | insert (x, y::ys) = 
	    (case order (x, y)
	      of LESS => x :: y :: ys
	       | EQUAL => x :: y :: ys
	       | GREATER => y :: (insert (x, ys)))
    in
	List.foldr insert nil
    end


val sorted = insertionSort

end
