(* library of SML functions used in Sail *)

fun sort_compare lessthan list =
    let
	fun insert lessthan n nil =
	    [n]
	  | insert lessthan n (n'::rest) =
	    if lessthan(n, n') then
		n::n'::rest
	    else
		n'::(insert lessthan n rest)
		
	fun isort lessthan alist nil = alist
	  | isort lessthan alist (n::rest) =
	    isort lessthan (insert lessthan n alist) rest
		  
    in
	isort lessthan [] list
    end

(* first for arrays *)
val array_null = List.null
fun array_split [] = ([], [])
  | array_split [item] = ([item], [])
  | array_split (item::rest) = ([item], rest)
val array_sort = sort_compare (op <)
val array_concat = (op @)

(* now for pairs *)
fun pair_one (a,b) = a
fun pair_two (a,b) = b
;
