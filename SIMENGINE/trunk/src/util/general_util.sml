structure GeneralUtil =
struct

datatype status =
	 SUCCESS 
       | FAILURE of string

(*TODO: use paths*)
fun filepath_split (filename) =
    let
	val fullpath = OS.FileSys.fullPath filename
	val split_path = String.fields (fn(c) => c = #"/") fullpath
    in
	((hd o rev) split_path,
	 String.concatWith "/" ((rev o tl o rev) split_path))
    end

fun flatten x = foldr (op @) nil x
fun flatmap f l = flatten (map f l)


fun curry1 f x = (fn(y)=> f(x,y))

fun curry2 f x = (fn(y)=> f(y,x))

fun array2list a = Array.foldr (op ::) [] a


fun addCount list =
    ListPair.zip (list, List.tabulate (length list, (fn(i)=>i)))


fun listSlice list (first, rest) =
    List.take(List.drop(list, first), rest-first)

fun vector2list vec = 
    Vector.foldr (fn (a,l) => a::l) [] vec


fun mapPairs f nil = nil
  | mapPairs f (x::nil) = nil
  | mapPairs f (x::y::rest) = (f (x,y)) :: (mapPairs f (y::rest))

fun applyOpt f NONE = NONE
  | applyOpt f (SOME x) = SOME (f x)

fun mapOpt f NONE = NONE
  | mapOpt f (SOME x) = SOME (map f x)


(*fun sort_compare lessthan list =
    let
	fun insert lessthan n nil =
	    [n]
	  | insert lessthan n (n'::rest) =
	    if lessthan(n, n') then
		n::n'::rest
	    else
		n'::(insert lessthan n rest)
		
	fun isort lessthan alist nil = alist
	  | isort lessthan alist ((p,l)::rest) =
	    isort lessthan (insert lessthan (p,l) alist) rest
		  
    in
	isort lessthan [] list
    end
*) (* commented out the above, below version should work for tuples of any size *)
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

fun sort_intmap elem2int list =
    let
	val zipped_pairs = ListPair.zip (list, map elem2int list)
	fun lessthan ((_,x),(_,y)) = x < y

	val (ordered, _) = ListPair.unzip(sort_compare lessthan zipped_pairs)
    in
	ordered
    end

fun intersection s1 s2
  = List.mapPartial (fn(s) => List.find (fn(s') => s=s') s2) s1

(*
    if String.isPrefix "DYNAMO__" name then
	let 
	    fun break_name n nil = [n]
	      | break_name n (c::nil) = [n ^ (Char.toString c)]
	      | break_name n ((#"_")::(#"_")::rest) = n :: (break_name "" rest)
	      | break_name n ((#"_")::(#"0")::rest) = break_name (n^"_") rest
	      | break_name n (c1::c2::rest) = break_name (n^(Char.toString c1)) 
							 (c2::rest)
	    val name = String.extract (name, 8, NONE)
	    val name = String.concatWith "." (break_name "" (explode name))
	in
	    name
	end
    else if String.isPrefix "DERIVED" name then
	name
    else if String.isPrefix "TABLE" name then
	"ADDR_" ^ (String.extract (name,6,NONE))
		      (* RKW - put in ADDR, but really want some type of mapping to 'real' expressions *)
    else raise UnexpectedInternalName before print ("Unexpected: " ^ name ^ "\n")
*)
fun real2str x = 
    let
	fun tilde2minus #"~" = #"-"
	  | tilde2minus c = c

    in
	String.implode (map tilde2minus (String.explode (Real.toString x)))
    end


fun int2str x = 
    let
	fun tilde2minus #"~" = #"-"
	  | tilde2minus c = c

    in
	String.implode (map tilde2minus (String.explode (Int.toString x)))
    end

fun intinf2str x = 
    let
	fun tilde2minus #"~" = #"-"
	  | tilde2minus c = c

    in
	String.implode (map tilde2minus (String.explode (IntInf.toString x)))
    end
	
	
fun bool2str x =
    if x then "true"
    else "false"

fun strToUpper string =
    String.map Char.toUpper string    
  
fun max nil = 0
  | max (n::nil) = n
  | max (n::rest) = 
    let val m = max rest 
    in if n > m 
       then n 
       else m 
    end

fun min nil = 0
  | min (n::nil) = n
  | min (n::rest) = 
    let val m = min rest 
    in if n < m 
       then n 
       else m 
    end

val uniq_id = ref 0
fun gen_id() = !uniq_id before uniq_id := (!uniq_id + 1)
 
(* val addListIndex : a' list -> (a' * int) list *)
fun addListIndex l = ListPair.zip (l,(List.tabulate (List.length l, fn(x)=>x)))

fun toUpper string =
    String.map Char.toUpper string

fun strcmpi (str1, str2) = 
    (toUpper str1) = (toUpper str2)

fun uniquify nil = nil
  | uniquify (node::rest) =
    let
	fun isin node nil = false
	  | isin node (node'::rest) =
	    (node=node') orelse (isin node rest)
    in		
	if isin node rest then
	    uniquify rest
	else
	    node :: (uniquify rest)
    end

fun list2str gen2str list =
    "(" ^ (String.concatWith ", " (map gen2str list)) ^ ")"
    
fun strlist2str list = list2str (fn(x)=>x) list

fun intlist2str intlist = list2str int2str intlist


(* binary operations *)



fun unzip3 abc_list =
    let fun unzip3_loop [] (aa,bb,cc) = (rev aa, rev bb, rev cc)
          | unzip3_loop ((a,b,c)::rest) (aa,bb,cc) = unzip3_loop rest (a::aa,b::bb,c::cc)
    in unzip3_loop abc_list ([],[],[])
    end
	
fun zip3 a b c = map (fn ((a,b),c) => (a,b,c)) 
                     (ListPair.zip (ListPair.zip (a, b), c))


(* string functions *)

(* take escape characters and add a slash in front of them *)
fun escapeString str =
    let
	fun escapeString_helper nil = ""
	  | escapeString_helper (#"\"" :: rest) = ("\\\"" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\\" :: rest) = ("\\\\" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\a" :: rest) = ("\\\a" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\b" :: rest) = ("\\\b" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\t" :: rest) = ("\\\t" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\n" :: rest) = ("\\\n" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\v" :: rest) = ("\\\v" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\f" :: rest) = ("\\\f" ^ (escapeString_helper rest))
	  | escapeString_helper (#"\r" :: rest) = ("\\\r" ^ (escapeString_helper rest))
	  | escapeString_helper (c :: rest) = (Char.toString c) ^ (escapeString_helper rest)
    in
	escapeString_helper (explode str)
    end


fun stringRev str =
    implode (rev (explode str))

fun isStringSuffix suffix str =
    String.isPrefix (stringRev suffix) (stringRev str)

fun listTripleUnzip nil = (nil, nil, nil)
  | listTripleUnzip ((a,b,c)::rest) =
    let
	val (d,e,f) = listTripleUnzip rest
    in
	(a::d, b::e, c::f)
    end


(* check if directory exists *)
fun isDir dir =
    OS.FileSys.isDir dir
    handle _ => false

(* check file if it exists - this function returns false on symbolic links *)
fun isFile file =
    (OS.FileSys.fileSize file;
     if isDir file then
	 false
     else 
	 true)
    handle _ => false

end
