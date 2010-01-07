structure StringLib =
struct

val strsfun = LibraryUtil.strsfun
and strs2boolfun = LibraryUtil.strs2boolfun

val TypeMismatch = DynException.TypeMismatch
and ValueError = DynException.ValueError
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_error (Printer.$ msg)

fun std_print exec args =
    KEC.UNIT before
    (case args of
	 [KEC.LITERAL (KEC.CONSTSTR s)] => print s
       | [arg] => print (PrettyPrint.kecexp2prettystr exec arg)
       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})


fun str_concat _ = strsfun op^

fun lstrip _ "" = ""
  | lstrip "" str = str
  | lstrip chars str =
    let
	val contains = Char.contains chars
	val substr = Substring.full str
	val leftsub = Substring.takel (Char.contains chars) substr
	val size = Substring.size leftsub
    in
	Substring.string (Substring.slice (substr, size, NONE))
    end

fun rstrip _ "" = ""
  | rstrip "" str = str
  | rstrip chars str =
    let
	val contains = Char.contains chars
	val substr = Substring.full str
	val rightsub = Substring.taker (Char.contains chars) substr
	val size = (Substring.size substr) - (Substring.size rightsub)
    in
	Substring.string (Substring.slice (substr, 0, SOME size))
    end

fun strip chars str = ((rstrip chars) o (lstrip chars)) str

fun str_lstrip _ = strsfun (fn (s1, s2) => lstrip s1 s2)
fun str_rstrip _ = strsfun (fn (s1, s2) => rstrip s1 s2)
fun str_strip _ = strsfun (fn (s1, s2) => strip s1 s2)

fun str_contains _ = strs2boolfun (fn (s1, s2) => String.isSubstring s2 s1)
fun str_startsWith _ = strs2boolfun (fn (s1, s2) => String.isPrefix s2 s1)
fun str_endsWith _ = strs2boolfun (fn (s1, s2) => String.isSuffix s2 s1)

(* Returns a new string with each occurance of a search substring replaced by a new substring. *)
fun replace str "" "" = str
  | replace str "" new =
    (* Replaces an empty search string at every opportunity: at the beginning, the end, and between each character. *)
    let
	fun replace_acc (acc: string list) (str: Substring.substring) =
	    if Substring.isEmpty str then
		String.concatWith "" (List.rev (new :: acc))
	    else
		replace_acc (String.str (Substring.sub (str, 0)) :: (new :: acc)) (Substring.slice (str, 1, NONE))
    in
	replace_acc nil (Substring.extract (str, 0, NONE))
    end
  | replace str old new = 
    let
	val oldn = String.size old
	fun replace_acc (acc: string list) (str: Substring.substring) =
	    if Substring.isEmpty str then
		String.concatWith "" (List.rev acc)
	    else if Substring.isPrefix old str then
		replace_acc (new :: acc) (Substring.slice (str, oldn, NONE))
	    else
		replace_acc (String.str (Substring.sub (str, 0)) :: acc) (Substring.slice (str, 1, NONE))
    in
	replace_acc nil (Substring.full str)
    end

fun str_replace _ args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR str), KEC.LITERAL (KEC.CONSTSTR old), KEC.LITERAL (KEC.CONSTSTR new)]
	=> KEC.LITERAL (KEC.CONSTSTR (replace str old new))
      | [a, b, c] =>
	raise TypeMismatch ("expected 3 strings but received " ^ (PrettyPrint.kecexp2nickname a) ^ ", " ^ (PrettyPrint.kecexp2nickname b) ^ ", and " ^ (PrettyPrint.kecexp2nickname c))
      | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)}

(* Returns a list of the component substrings of a string. *)
fun split "" = 
    (fn "" => [""] 
      | str => map String.str (explode str))
  | split sep =
    let
	val sepn = String.size sep
	val find_sep = Substring.position sep
	val isEmpty = Substring.isEmpty
	fun advance substr = Substring.slice (substr, sepn, NONE)
	fun split_acc (acc: Substring.substring list, substr: Substring.substring) =
	    if isEmpty substr then 
		map Substring.string (List.rev acc)
	    else 
		let val (prefix, suffix) = find_sep substr
		in
		    if isEmpty suffix then
			split_acc (prefix :: acc, suffix)
		    else if Substring.size suffix = sepn andalso Substring.isPrefix sep suffix then
			(* Accumulates an extra empty substring when the separator appears at the end. *)
			split_acc (advance suffix :: prefix :: acc, advance suffix)
		    else
			split_acc (prefix :: acc, advance suffix)
		end
    in
     fn "" => [""]
      | str => split_acc (nil, Substring.full str)
    end

fun str_split exec args =
    case args of 
	[KEC.LITERAL (KEC.CONSTSTR s1), KEC.LITERAL (KEC.CONSTSTR s2)] 
	=> exec(KEC.list2kecvector (map (fn (s) => (KEC.LITERAL (KEC.CONSTSTR s)))
					(split s2 s1)))
      | [a, b] 
	=> raise TypeMismatch ("expected 2 strings but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}
		      
fun makeTranslation (old: string, new: string) =
    let
	val bindings = map (fn (c1,c2) => (String.str c1, SOME c2)) 
			   (ListPair.zip (explode old, explode new))
	val chrMap = CharMap.mkCharMap {default=NONE, bindings=bindings}
	val mapChr = CharMap.mapChr chrMap
    in
	fn c =>
	   case mapChr c of
	       SOME c' => c'
	     | NONE => c
    end

(* Returns a new string with characters substituted. *)
fun translate ("", "") = (fn text => text)
  | translate (old, new) =
    let val trans = String.translate (String.str o (makeTranslation (old, new)))
    in 
     fn "" => "" 
      | text => trans text
    end

fun str_translate _ args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR str), KEC.LITERAL (KEC.CONSTSTR old), KEC.LITERAL (KEC.CONSTSTR new)] => 
	if String.size old = String.size new then
	    KEC.LITERAL (KEC.CONSTSTR (translate (old, new) str))
	else raise ValueError ("expected 2 strings having the same length")
      | [a, b, c] =>
	raise TypeMismatch ("expected 3 strings but received " ^ (PrettyPrint.kecexp2nickname a) ^ ", " ^ (PrettyPrint.kecexp2nickname b) ^ ", and " ^ (PrettyPrint.kecexp2nickname c))
      | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)}


fun std_substring _ args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR s), KEC.LITERAL (KEC.CONSTREAL pos), KEC.LITERAL (KEC.CONSTREAL len)]
	=> (KEC.LITERAL (KEC.CONSTSTR (String.substring (s, ((Real.round pos) - 1), (Real.round len))))
	    (* TODO: raise an exception *)
	    handle _ => KEC.UNDEFINED before error ("substring index out of bounds"))
      | [s, pos, len] =>
	raise TypeMismatch ("expected a string and two numbers but received " ^ (PrettyPrint.kecexp2nickname s) ^ ", " ^ (PrettyPrint.kecexp2nickname pos) ^ ", and " ^ (PrettyPrint.kecexp2nickname len))
      | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)}

fun std_warning _ args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR s)] =>
	KEC.UNIT before Logger.log_warning (Printer.$ s)
      | [s] =>
	raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname s))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_notice _ args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR s)] =>
	KEC.UNIT before Logger.log_notice (Printer.$ s)
      | [s] =>
	raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname s))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="print", operation=std_print},
	       {name="strconcat", operation=str_concat},
	       {name="substring", operation=std_substring},
	       {name="warning", operation=std_warning},
	       {name="notice", operation=std_notice},
	       {name="str_contains", operation=str_contains},
	       {name="str_startsWith", operation=str_startsWith},
	       {name="str_endsWith", operation=str_endsWith},
	       {name="str_strip", operation=str_strip},
	       {name="str_lstrip", operation=str_lstrip},
	       {name="str_rstrip", operation=str_rstrip},
	       {name="str_replace", operation=str_replace},
	       {name="str_translate", operation=str_translate},
	       {name="str_split", operation=str_split}]

end
