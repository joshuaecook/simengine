(*TODO: clean up this mess *)
structure StdFun =
struct

exception Empty
exception Subscript
exception InternalError
exception LogError

fun flatten x = foldr (op @) nil x
fun flatmap f l = flatten (map f l)


fun curry1 f x = (fn(y)=> f(x,y))

fun curry2 f x = (fn(y)=> f(y,x))


fun addCount list =
    ListPair.zip (list, List.tabulate (length list, (fn(i)=>i)))

fun sum l = foldl (op +) 0 l


fun mapPairs f nil = nil
  | mapPairs f (x::nil) = nil
  | mapPairs f (x::y::rest) = (f (x,y)) :: (mapPairs f (y::rest))


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
    handle e => DynException.checkpoint "StdFun.sort_compare" e

fun sort_intmap elem2int list =
    let
	val zipped_pairs = ListPair.zip (list, map elem2int list)
	fun lessthan ((_,x),(_,y)) = x < y

	val (ordered, _) = ListPair.unzip(sort_compare lessthan zipped_pairs)
    in
	ordered
    end

fun intersection s1 s2 = 
    List.mapPartial (fn(s) => List.find (fn(s') => s=s') s2) s1

fun intersection_by_fun cmp_fun (s1,s2) = 
    List.mapPartial (fn(s) => List.find (fn(s') => cmp_fun (s,s')) s2) s1

fun lbrack (c) =
    if c = "[" then
	"_0"
    else
	c
fun rbrack (c) =
    if c = "]" then
	"_1"
    else
	c

fun period (c) = 
    if c = "." then
	"_2"
    else
	c

fun underscore (c) = 
    if c = "_" then
	"_3"
    else
	c

fun dash (c) =
    if c = "-" then
	"_4"
    else
	c

fun space (c) =
    if c = " " then
	"_5"
    else
	c

fun lparen (c) =
    if c = "(" then
	"_6"
    else
	c
fun rparen (c) =
    if c = ")" then
	"_7"
    else
	c

fun plus (c) = 
    if c = "+" then "_8" else c


fun stringmap f s =
    String.concatWith "" (map f (map Char.toString (explode s)))


exception IllegalEscapeSequence

(* Returns the value of a string literal by decoding its printed representation. 
   String literals may contain escaped codes, indicated by a backslash, to
   represent special or "unprintable" characters.
 *)
fun fromString str =
    let
	val read = Char.scan Substring.getc

	(* Returns a char list by decoding an escaped sequence of chars. *)
	fun escape substr =
	    if Substring.isEmpty substr then raise IllegalEscapeSequence
	    else
		case read substr of
		    SOME (#"$", suffix) => ([#"$"], suffix)

		    (* Retains the escape char before any character that isn't recognized as a special escape sequence. *)
		  | SOME (c, suffix) => ([c, #"\\"], suffix)

		  (* The first character is non-printable or starts an "illegal" (to SML) escape sequence. *)
		  | NONE => 
		    let
			val code = Char.ord (Substring.sub (substr, 0))
		    in 
			(* Treats escaped non-printable characters as literal characters. *)
			if code < 0x20 orelse 0x7E < code then
			    ([Char.chr code], Substring.slice (substr, 1, NONE))
			else raise IllegalEscapeSequence
		    end
		    
	(* Returns a string by scanning a substring and decoding escape sequences. *)
	fun chars (acc, substr) =
	    if Substring.isEmpty substr then implode (List.rev acc)
	    else
		case read substr of
		    SOME (c, suffix) => chars (c :: acc, suffix)

		  (* The first character is non-printable or starts an "illegal" (to SML) escape sequence. *)
		  | NONE =>
		    let 
			val (c, suffix) = (Substring.sub (substr, 0), Substring.slice (substr, 1, NONE))
		    in 
			case c of
			    #"\\" =>
			    (* Looking past the backslash, attempt to decode and accumulate an escape sequence. *)
			    let val (escaped, suffix') = escape suffix in
				chars (escaped @ acc, suffix')
			    end
			  | _ => raise IllegalEscapeSequence
		    end
    in
	chars (nil, Substring.full str)
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
    handle e => DynException.checkpoint "StdFun.inStr" e


fun strMatchCount substr string =
    let
	val l1 = String.size string
	val l2 = String.size substr
    in
	if l2 > l1 then (* if string is too short, then there are none *)
	    0
	else
	    length (List.filter (* grab all possible substrings from s1 and see which matches s2 *)
			(fn(s)=>s=substr)
			(List.tabulate (l1-l2+1, (fn(x)=>substring(string,x,l2)))))
    end
    handle e => DynException.checkpoint "StdFun.strMatchCount" e
    

fun isFixed name =
    (strMatchCount "." name) = 0
    andalso (strMatchCount "[" name) = 0
    andalso (strMatchCount "]" name) = 0
    andalso (strMatchCount "-" name) = 0
    andalso (strMatchCount " " name) = 0
    andalso (strMatchCount "(" name) = 0
    andalso (strMatchCount ")" name) = 0
    andalso (strMatchCount "+" name) = 0
    andalso (strMatchCount "_" name) = (foldl (op +) 0 (map (fn(s) => strMatchCount s name) ["_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8"]))


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

fun str2real x = 
    case Real.fromString x of
	SOME v => v
      | NONE => DynException.stdException(("Can't convert str '"^x^"' to real"), "StdFun.str2real", Logger.INTERNAL)

fun int2str x = 
    let
	fun tilde2minus #"~" = #"-"
	  | tilde2minus c = c

    in
	String.implode (map tilde2minus (String.explode (Int.toString x)))
    end

fun real2expstr x = 
    let
	val {man,exp} = Real.toManExp x
    in
	(real2str man) ^ "*pow(2," ^ (int2str exp) ^ ")"
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
    
fun log2 x = 
    (if x <= 0.0 then
	 DynException.stdException ("Trying to take the log of "^(real2str x)^" which will result in either a complex or infinite result", "StdFun.log2", Logger.INTERNAL)
     else
	 (Math.log10 x) / (Math.log10 2.0))
    handle e => (DynException.log "StdFun.log2" e;
		 raise e)

fun next_pow2 x = 
    (if x < 0.0 then
	 DynException.stdException ("Can't take the next pow2 if the input (x="^(real2str x)^") is less than zero", "StdFun.next_pow2", Logger.INTERNAL)
     else if Real.?= (x,0.0) then
	 0.0
     else
	 Math.pow (2.0, Real.realCeil(log2 x)))
    handle e => (DynException.log "StdFun.next_pow2" e;
		 raise e)

fun repeat value times =
    (if times < 0 then
	 DynException.stdException ("Can't repeat less than zero ("^(int2str times)^") times", "StdFun.repeat", Logger.INTERNAL)
     else
	List.tabulate (times, (fn(n)=>value)))
    handle e => (DynException.log "StdFun.repeat" e;
		 raise e)

(* count returns a list from low to high, if it can't, it returns an empty list *)
fun count (low, high) =
    if low <= high then
	List.tabulate (high-low+1, (fn(i)=> i + low))
    else
	[]
	(*raise Subscript before print ("Error StdFun.count: can't count from <"^(int2str low)^"> to <"^(int2str high)^">\n")*)
  
fun max nil = DynException.stdException("Trying to find the maximum of an empty list", "StdFun.max", Logger.INTERNAL)
  | max (n::nil) = n
  | max (n::rest) = 
    let val m = max rest 
    in if n > m 
       then n 
       else m 
    end

fun min nil = DynException.stdException("Trying to find the minimum of an empty list", "StdFun.min", Logger.INTERNAL)
  | min (n::nil) = n
  | min (n::rest) = 
    let val m = min rest 
    in if n < m 
       then n 
       else m 
    end

fun rmax nil : real = DynException.stdException("Trying to find the maximum of an empty list", "StdFun.rmax", Logger.INTERNAL)
  | rmax (n::nil) = n
  | rmax (n::rest) = 
    let val m = rmax rest 
    in if n > m 
       then n 
       else m 
    end

fun rmin nil : real = DynException.stdException("Trying to find the minimum of an empty list", "StdFun.rmin", Logger.INTERNAL)
  | rmin (n::nil) = n
  | rmin (n::rest) = 
    let val m = rmin rest 
    in if n < m 
       then n 
       else m 
    end

fun flatten x = foldr (op @) nil x

val uniq_id = ref 0
fun gen_id() = !uniq_id before uniq_id := (!uniq_id + 1)
 
fun list_remove (elem, list) =
   let
       val counting_list = List.tabulate (length list, (fn(x)=>x))

       fun drop_num (nil, _) = nil
         | drop_num (list, 0) = tl list
         | drop_num (list, num) = (hd list) :: (drop_num (tl list, num-1))
   in
       case List.find (fn(e,_) => e = elem)
                      (ListPair.zip (list, counting_list)) of
           SOME (_, pos) => drop_num (list, pos)
         | NONE => list
   end


fun listOr blist = 
    let
	fun ior (b1,b2) = b1 orelse b2;
    in 
	foldl ior false blist
    end

fun listAnd blist = 
    let
	fun iand (b1,b2) = b1 andalso b2;
    in 
	foldl iand true blist
    end

fun transpose nil = nil 
  | transpose (nil::_) = nil
  | transpose list =
    (map hd list) :: (transpose (map tl list))

(* val addListIndex : a' list -> (a' * int) list *)
fun addListIndex l = ListPair.zip (l,(List.tabulate (List.length l, fn(x)=>x)))

fun toUpper string =
    String.map Char.toUpper string

fun toLower str = 
    String.map Char.toLower str

fun strcmpi (str1, str2) = 
    (toUpper str1) = (toUpper str2)

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

(* 5-7-07 - refactor, producing a generic uniquify *)
(*fun uniquify nil = nil
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
    end*)

fun list2str gen2str list =
    "(" ^ (String.concatWith ", " (map gen2str list)) ^ ")"
    
fun strlist2str list = list2str (fn(x)=>x) list

fun intlist2str intlist = list2str int2str intlist


(* binary operations *)

(* exp2(x) = 2^x where Int -> Int *)
fun exp2 n = Real.round (Math.pow(2.0,(Real.fromInt n)))

fun bitcmp bin =
    map not bin

fun posint2binary 1 int = 
    [int > 0]
  | posint2binary bits int = 
    let
	val bit_index = exp2 (bits - 1)
    in
	if int >= bit_index then
	    true::(posint2binary (bits-1) (int-bit_index))
	else
	    false::(posint2binary (bits-1) int)
    end

fun binary2int bin = 
    foldl
	(fn((a,n),b)=> if a then b + exp2((List.length bin)-n-1) else b)
	0
	(addListIndex bin)

fun twoscomp bin =
    let 
	val bits = List.length bin
    in
	posint2binary bits ((binary2int (bitcmp bin)) + 1)
    end

fun int2binary bits int =
    if int >= 0 then
	posint2binary bits int
    else
	twoscomp (posint2binary bits (~int))

fun bin2str bin =
    foldl
	(fn(a,b)=>if a then (b ^ "1") else (b ^ "0"))
	""
	bin

fun int2binstr bits int = bin2str (int2binary bits int)

fun unzip3 abc_list =
    let fun unzip3_loop [] (aa,bb,cc) = (rev aa, rev bb, rev cc)
          | unzip3_loop ((a,b,c)::rest) (aa,bb,cc) = unzip3_loop rest (a::aa,b::bb,c::cc)
    in unzip3_loop abc_list ([],[],[])
    end
	
fun zip3 a b c = map (fn ((a,b),c) => (a,b,c)) 
                     (ListPair.zip (ListPair.zip (a, b), c))

(* going to redo these with some error messages *)
fun tl l = 
    if List.length l = 0 orelse l = nil then
	DynException.stdException ("Trying to take the tail of an empty list", "StdFun.tl", Logger.INTERNAL)
    else
	List.tl l

fun hd l =
    if List.length l = 0 then
	DynException.stdException ("Trying to take the head of an empty list", "StdFun.hd", Logger.INTERNAL)
    else
	List.hd l

fun nth (l,i) =
    if List.length l = 0 then
	 DynException.stdException ("Trying to extract element #"^(int2str i)^" from an empty list (zero indexed)", "StdFun.nth", Logger.INTERNAL)
    else if i < 0 orelse i >= (List.length l) then
	 DynException.stdException ("Trying to extract element #"^(int2str i)^" from a list of length "^(int2str (List.length l))^" (zero indexed)", "StdFun.nth", Logger.INTERNAL)
    else
	List.nth (l,i) handle e => (DynException.log "StdFun.nth" e;
				    raise e)

fun take (l,i) = 
    if List.length l = 0 then
	DynException.stdException ("Trying to extract elements #0 through #"^(int2str i)^" from an empty list (zero indexed)", "StdFun.take", Logger.INTERNAL)

    else if i < 0 orelse i > (List.length l) then
	DynException.stdException ("Trying to extract elements #0 through #"^(int2str i)^" from a list of length "^(int2str (List.length l))^" (zero indexed)", "StdFun.take", Logger.INTERNAL)
    else
	List.take (l,i) handle e => (DynException.log "StdFun.take" e;
				     raise e)

fun drop (l,i) = 
    if List.length l = 0 then
	DynException.stdException ("Trying to extract element #"^(int2str i)^" from an empty list (zero indexed)", "StdFun.drop", Logger.INTERNAL)
    else if i < 0 orelse i > (List.length l) then
	DynException.stdException ("Trying to extract elements starting with  #"^(int2str i)^" from a list of length "^(int2str (List.length l))^" (zero indexed)", "StdFun.drop", Logger.INTERNAL)

    else
	List.drop (l,i) handle e => (DynException.log "StdFun.drop" e;
				     raise e)

fun substring (s, i, j) = 
    let
	val first = i
	val last = i+j-1
	val fstr = int2str first
	val lstr = int2str last
    in
	if String.size s = 0 then
	    DynException.stdException ("Trying to extract elements #"^fstr^" to #"^lstr^" from an empty string (zero indexed)", "StdFun.substring", Logger.INTERNAL)
	else if first < 0 then
	    DynException.stdException ("Trying to extract element #"^fstr^" which is less than zero from the string '"^s^"'", "StdFun.substring", Logger.INTERNAL)
	else if last >= (String.size s) then
	    DynException.stdException ("Trying to extract elements #"^fstr^" to #"^lstr^" which are not fully contained in the string '"^s^"'", "StdFun.substring", Logger.INTERNAL)
	else if first > last then
	 DynException.stdException ("Must extract at least one element in the string <"^s^">, instead trying to extract elements #"^fstr^" to #"^lstr, "StdFun.substring", Logger.INTERNAL)
	else
	    String.substring (s, i, j) handle e => (DynException.log "StdFun.substring" e; raise e)
    end

fun extract (s, i, opt) =
    (case opt of
	 SOME j => substring (s, i, j)
       | NONE => substring (s, i, (String.size s)-i))
    handle e => (DynException.log "StdFun.extract" e; raise e)

fun stringRev str =
    implode (rev (explode str))

fun isStringSuffix suffix str =
    String.isPrefix (stringRev suffix) (stringRev str)

(* Conversion functions for internal signal names *)
fun fixname name = 
    if isFixed name then
	name
    else
	(stringmap (lbrack o rbrack o period o dash o space o underscore o lparen o rparen o plus) name)

fun unfixname name =
    (if isFixed name then
	 let 
	     fun break_name n nil = n
	       | break_name n (c::nil) = n ^ (Char.toString c)
	       | break_name n ((#"_")::(#"0")::rest) = break_name (n ^ "[") rest
	       | break_name n ((#"_")::(#"1")::rest) = break_name (n ^ "]") rest
	       | break_name n ((#"_")::(#"2")::rest) = break_name (n ^ ".") rest
	       | break_name n ((#"_")::(#"3")::rest) = break_name (n ^ "_") rest
	       | break_name n ((#"_")::(#"4")::rest) = break_name (n ^ "-") rest
	       | break_name n ((#"_")::(#"5")::rest) = break_name (n ^ " ") rest
	       | break_name n ((#"_")::(#"6")::rest) = break_name (n ^ "(") rest
	       | break_name n ((#"_")::(#"7")::rest) = break_name (n ^ ")") rest
	       | break_name n ((#"_")::(#"8")::rest) = break_name (n ^ "+") rest
	       | break_name n (c1::c2::rest) = break_name (n^(Char.toString c1))
							  (c2::rest)
					       
	 (* RKW - 11-15-07 - can't figure out why the line below was in this function.  Regardless, it was causing exceptions so it is commented out now. *)
	     (*val name = extract (name, 7, NONE)*)
	 in
	     break_name "" (explode name)
	 end
     else
	 name)
    handle e => DynException.checkpoint ("StdFun.unfixname ["^name^"]") e
	


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
    handle e => DynException.checkpoint "StdFun.findStr" e

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
    handle e => DynException.checkpoint "StdFun.repStr" e


(* manipulate directories and system names *)
fun formatSysName name = 
    let
	fun isSlash c = 
	    c = #"/"
	    
	fun isDot c = 
	    c = #"."
    in
	hd(String.tokens isDot (hd(rev(String.tokens isSlash name))))
    end
    
fun name2work_dir name =
    let
	val sysname = formatSysName name
	val dir = DynamoOptions.getStringSetting "targetlocation"

	val work_dir = 	if DynamoOptions.isFlagSet "appendsysname" then
			    if dir = "" then
				sysname
			    else
				dir ^ "_" ^ sysname
			else
			    dir
    in
	if String.isSuffix Globals.extension work_dir then
	    work_dir
	else
	    work_dir ^ Globals.extension
    end

(* finds environmental variables and expands them out *)
fun expand_env_variables s =
    (if inStr(s, "$") then
	let
	    val _ = if hd (findStr(s, "$")) = (String.size s - 1) then (* if it is the last character, raise an error *)
			DynException.stdException (("A $ can't be the last character where an environmental variable could be expanded ('"^s^"')"),
						   "StdFun.expand_env_variables",
						   Logger.INTERNAL)
		    else
			()

	    open Printer

	    fun isEnvVar var = 
		case OS.Process.getEnv var of
		    SOME reg => true
		  | NONE => false

	    fun var2str var = 
		case OS.Process.getEnv var of
		    SOME reg => reg
		  | NONE => DynException.stdException (("Can't convert env var '"^var^"' to string"),"StdFun.expand_env_variables", Logger.INTERNAL)

	    val envs = List.filter isEnvVar (map (fn(l)=>substring(s,hd(findStr(s,"$"))+1,l)) (count (1,(String.size s - (hd(findStr(s,"$"))) - 1))))

	    val env = if List.length envs = 0 then
			  (DynException.stdException (("No environmental variable is defined ('"^s^"')"),"StdFun.expand_env_variables", Logger.INTERNAL);
			   "")
		      else if List.length envs > 1 then
			  (Logger.log_warning ($("Multiple environmental variables found in '"^s^"', using '"^(hd(envs))^"'"));
			   hd(envs))
		      else
			  hd(envs)
			  
	    val s' = repStr (s,"$"^env,var2str(env))
	in
	    expand_env_variables s'
	end
    else
	s)
    handle e => DynException.checkpoint "StdFun.expand_env_variables" e
	
(* This function is misnamed - it always sets the bits to the max bits *)
fun truncate_prec max_bits {sign, bits, frac} =
    let
	val new_frac = if max_bits > bits then
			   frac
		       else
			   (max_bits-bits)+frac
	val new_frac = if new_frac < 0 then
			   0
		       else
			   new_frac
    in
	{sign=sign, bits=max_bits, frac=new_frac}
    end

(* Here, the original number of bits is preserved, when possible *)
fun truncate_prec' max_bits {sign, bits, frac} =
    let
	val new_frac = if max_bits > bits then
			   frac
		       else
			   (max_bits-bits)+frac
	val new_frac = if new_frac < 0 then
			   0
		       else
			   new_frac

	val new_bits = min [max_bits, bits]
    in
	{sign=sign, bits=new_bits, frac=new_frac}
    end

fun truncate_output_prec bitinfo = 
    truncate_prec (DynamoOptions.getIntegerSetting "iobitwidth") bitinfo


(* sort_outputs - special function to sort only outputs so that software and 
and hardware outputs always match *)
fun sort_outputs list = 
    let
	open Printer
	
	val sorted_list = sort_compare (fn(a,b)=>String.< (a,b)) list
	val (matched, unmatched) = List.partition (fn(s)=>s="t") sorted_list
	val _ = DynException.assert (List.length matched = 1) ($"There should always be one output with name t")
    in
	matched @ unmatched
    end
    handle e => DynException.checkpoint ("StdFun.sort_outputs [" ^ (strlist2str list) ^ "]") e

fun sort_outputs_with_visibility outputs =
    let
	open Printer

	val sorted = sort_compare (fn ({name=a,condition=_},{name=b,condition=_}) => String.<(a,b)) outputs
	val (ts, others) = List.partition (fn {name=name,condition=_} => "t"=name) sorted
	val _ = DynException.assert (List.length ts = 1) ($"There should always be one output with name t")
    in
	ts @ others
    end
    handle e => DynException.checkpoint ("StdFun.sort_outputs_with_visibility") e

(* compute the addressability required to represent a given number *)
fun num2addressability depth = 
    (if depth >= 1 then
	 (Real.ceil (log2 (next_pow2 (Real.fromInt depth))))
     else
	 DynException.stdException ("Input has to be >= 1", "StdFun.num2addressability", Logger.INTERNAL))
    handle e => DynException.checkpoint "StdFun.num2addressability" e

(* find a file in the path - returns a string option *)
fun get_file name = 
    let
	val {isAbs, vol, arcs} = OS.Path.fromString name

	fun isValidFile file = GeneralUtil.isFile file orelse 
			       (OS.FileSys.isLink file handle OS.SysErr v => false)
	    handle e => DynException.checkpoint "StdFun.get_file.isValidFile" e

    in
	if isAbs then
	    if isValidFile name then
		SOME name
	    else
		NONE
	else
	    let
		val possibleFiles =  (* return a list of possible file names *)
		    map
			(fn(p)=> p ^ "/" ^ name)
			(DynamoOptions.getStringVectorSetting "sourcepath")				    
	    in
		List.find 
		    (fn(file)=> isValidFile file)
		    possibleFiles
	    end
    end
    handle e => DynException.checkpoint "StdFun.get_file" e
    
end
