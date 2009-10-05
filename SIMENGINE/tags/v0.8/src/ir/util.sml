structure Util =
struct

exception InternalError

fun log s = 
    print (s ^ "\n")

fun r2s x = 
    let
	fun tilde2minus #"~" = #"-"
	  | tilde2minus c = c

    in
	String.implode (map tilde2minus (String.explode (Real.toString x)))
    end


fun i2s x = 
    let
	fun tilde2minus #"~" = #"-"
	  | tilde2minus c = c

    in
	String.implode (map tilde2minus (String.explode (Int.toString x)))
    end

fun b2s x =
    if x then "true"
    else "false"

fun l2s (l: string list) =
    "[" ^ (String.concatWith ", " l) ^ "]"

fun sum l = foldl (op +) 0 l

fun toUpper string =
    String.map Char.toUpper string

fun toLower str = 
    String.map Char.toLower str

fun strcmpi (str1, str2) = 
    (toUpper str1) = (toUpper str2)

fun same nil = true
  | same [elem] = true
  | same (elem1::elem2::rest) =
    (elem1 = elem2) andalso (same (elem2::rest))

fun addCount list =
    ListPair.zip (list, List.tabulate (length list, (fn(i)=>i)))

fun uniquify_by_fun cmp_fun nil = nil
  | uniquify_by_fun cmp_fun (node::rest) =
    let
	fun isin node nil = false
	  | isin node (node'::rest) =
	    cmp_fun (node,node') orelse (isin node rest)
    in		
	if isin node rest then
	    uniquify_by_fun cmp_fun rest
	else
	    node :: (uniquify_by_fun cmp_fun rest)
    end

fun uniquify nodes = 
    uniquify_by_fun (fn(a,b)=>a=b) nodes

fun tl l = 
    if List.length l = 0 orelse l = nil then
	DynException.stdException ("Trying to take the tail of an empty list", "Util.tl", Logger.INTERNAL)
    else
	List.tl l

fun hd l =
    if List.length l = 0 then
	DynException.stdException ("Trying to take the head of an empty list", "Util.hd", Logger.INTERNAL)
    else
	List.hd l

fun nth (l,i) =
    if List.length l = 0 then
	 DynException.stdException ("Trying to extract element #"^(i2s i)^" from an empty list (zero indexed)", "Util.nth", Logger.INTERNAL)
    else if i < 0 orelse i >= (List.length l) then
	 DynException.stdException ("Trying to extract element #"^(i2s i)^" from a list of length "^(i2s (List.length l))^" (zero indexed)", "Util.nth", Logger.INTERNAL)
    else
	List.nth (l,i) handle e => (DynException.log "Util.nth" e;
				    raise e)
fun take (l,i) = 
    if List.length l = 0 then
	DynException.stdException ("Trying to extract elements #0 through #"^(i2s i)^" from an empty list (zero indexed)", "Util.take", Logger.INTERNAL)

    else if i < 0 orelse i > (List.length l) then
	DynException.stdException ("Trying to extract elements #0 through #"^(i2s i)^" from a list of length "^(i2s (List.length l))^" (zero indexed)", "Util.take", Logger.INTERNAL)
    else
	List.take (l,i) handle e => (DynException.log "Util.take" e;
				     raise e)

fun drop (l,i) = 
    if List.length l = 0 then
	DynException.stdException ("Trying to extract element #"^(i2s i)^" from an empty list (zero indexed)", "Util.drop", Logger.INTERNAL)
    else if i < 0 orelse i > (List.length l) then
	DynException.stdException ("Trying to extract elements starting with  #"^(i2s i)^" from a list of length "^(i2s (List.length l))^" (zero indexed)", "Util.drop", Logger.INTERNAL)

    else
	List.drop (l,i) handle e => (DynException.log "Util.drop" e;
				     raise e)

fun substring (s, i, j) = 
    let
	val first = i
	val last = i+j-1
	val fstr = i2s first
	val lstr = i2s last
    in
	if String.size s = 0 then
	    DynException.stdException ("Trying to extract elements #"^fstr^" to #"^lstr^" from an empty string (zero indexed)", "Util.substring", Logger.INTERNAL)
	else if first < 0 then
	    DynException.stdException ("Trying to extract element #"^fstr^" which is less than zero from the string '"^s^"'", "Util.substring", Logger.INTERNAL)
	else if last >= (String.size s) then
	    DynException.stdException ("Trying to extract elements #"^fstr^" to #"^lstr^" which are not fully contained in the string '"^s^"'", "Util.substring", Logger.INTERNAL)
	else if first > last then
	 DynException.stdException ("Must extract at least one element in the string <"^s^">, instead trying to extract elements #"^fstr^" to #"^lstr, "Util.substring", Logger.INTERNAL)
	else
	    String.substring (s, i, j) handle e => (DynException.log "Util.substring" e; raise e)
    end


(* inStr - returns boolean if s2 exists within s1 *)
fun inStr (s1, s2) =
    let
	val l1 = String.size s1
	val l2 = String.size s2
    in
	if l2 > l1 then (* if s1 is too short, then it has to be false *)
	    false
	else
	    List.exists (* grab all possible substrings from s1 and see if anyone matches s2 *)
		(fn(s)=>s=s2)
		(List.tabulate (l1-l2+1, (fn(x)=>substring(s1,x,l2))))
    end
    handle e => DynException.checkpoint "Util.inStr" e


(* return the locations of the s2 inside s1 *)
fun findStr (s1, s2) : int list=
    let
	val l1 = String.size s1
	val l2 = String.size s2
    in
	if inStr (s1, s2) then
	    map (fn(s,c)=>c)
		(List.filter (* grab all possible substrings from s1 and see if anyone matches s2 *)
		     (fn(s,c)=>s=s2)
		     (List.tabulate (l1-l2+1, (fn(x)=>(substring(s1,x,l2),x)))))
	else 
	    []
    end
    handle e => DynException.checkpoint "Util.findStr" e

(* replace all occurrences of string s2 in s1 with the new string s3 *)
fun repStr (s1, s2, s3) =
    (if inStr (s1, s2) then
	 let
	     val index = hd (findStr (s1, s2))
	     val new_str = (implode (take (explode s1,index))) ^
			   s3 ^
			   (implode (drop (explode s1,index+(String.size s2))))
	 in
	     repStr (new_str, s2, s3)
	 end
     else
	 s1)
    handle e => DynException.checkpoint "Util.repStr" e

fun flatten x = foldr (op @) nil x
fun flatmap f l = flatten (map f l)

(* Set Contructs  *)
fun add_to_set (l, elem) = 
    case List.find (fn(e)=> e=elem) l of
	SOME v => l
      | NONE => elem::l

fun inSet (l, elem) = 
    List.exists (fn(e)=> e = elem) l

fun concatSets (l1, l2) = 
    foldl (fn(e, l)=> add_to_set (l, e)) l1 l2

fun intersection (s1, s2) = 
    List.mapPartial (fn(s) => List.find (fn(s') => s=s') s2) s1

end
