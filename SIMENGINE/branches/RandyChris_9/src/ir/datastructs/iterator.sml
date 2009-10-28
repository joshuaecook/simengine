signature ITERATOR =
sig
    (* Terms are indexed by an iterator to refer to
     * a specific value or set of values. *)
    datatype iteratorindex
      (* An index relative to the current value.
       * The (RELATIVE 0) index is the common means of 
       * referring to the "now" value. *)
      = RELATIVE of int
      (* The (ABSOLUTE 0) index is the common means of 
       * referring to the initial value. *)
      | ABSOLUTE of int
      | RANGE of (int * int)
      | LIST of int list
      | ALL
				     
    type iterator = (Symbol.symbol * iteratorindex)

    (* Useful functions *)
    val iterator2str : iterator -> string
    val iterators2str : iterator list -> string
    val iterator2c_str : iterator -> string (* C writer for iterator *)
    val iterators2c_str : iterator list -> string (* C writer for multiple iterator *)
    val iter_equiv : (iterator * iterator) -> bool

    val postProcessOf : string -> Symbol.symbol   
    val updateOf : string -> Symbol.symbol   

end
structure Iterator : ITERATOR =
struct

val i2s = Util.i2s

fun eventOf iter = Symbol.symbol ("event_" ^ iter )
fun postProcessOf iter = Symbol.symbol ("pp_" ^ iter)
fun updateOf iter = Symbol.symbol ("update_" ^ iter)

datatype iteratorindex = ALL
		       | ABSOLUTE of int
		       | RELATIVE of int
		       | RANGE of (int * int)
		       | LIST of int list

type iterator = (Symbol.symbol * iteratorindex)

fun iterator2str (iterator as (sym, i))= 
    let
	val str = Util.removePrefix (Symbol.name sym)
    in
	case i
	 of ALL => str ^ "=:"
	  | ABSOLUTE v => str ^ "=" ^ (i2s v)
	  | RELATIVE v => str ^ (if v = 0 then
				     ""
				 else if v < 0 then 
				     i2s v 
				 else 
				     "+" ^ (i2s v))
	  | RANGE (v1, v2) => str ^ "=" ^ (i2s v1) ^ ":" ^ (i2s v2)
	  | LIST l => str ^ "=(" ^ (String.concatWith "," (map i2s l)) ^ ")"
    end

fun iterators2str iterators =
    "[" ^ (String.concatWith "," (map iterator2str iterators)) ^ "]"


fun iterator2c_str (iterator as (sym, i)) =
    let
	val str = Symbol.name sym
    in
	case i of
	    ALL => ""
	  | ABSOLUTE i => (i2s i)
	  | RELATIVE i => str ^ (if i = 0 then "" else "+" ^ (i2s i))
	  | RANGE _ => DynException.stdException("Currently, an iterator range can't be specified.  Instead, use one or more symbols with absolute indices.", "Iterator.iterator2c_str", Logger.INTERNAL)
	  | LIST _ => DynException.stdException("Currently, an iterator list can't be specified.  Instead, use one or more symbols with absolute indices.", "Iterator.iterator2c_str", Logger.INTERNAL)
    end

fun iterators2c_str iterators =
    if List.length iterators > 0 then
	String.concat (map (fn(iter)=> "[" ^ (iterator2c_str iter) ^ "]") iterators)
	(*"[" ^ (String.concatWith "," (map iterator2c_str iterators)) ^ "]"*)
    else
	""



fun iter_equiv (i1 as (s1, t1), i2 as (s2, t2)) =
    (* check the symbol *)
    s1 = s2
    andalso
    (case (t1, t2)
      of (ALL, ALL) => true
       | (ABSOLUTE a, ABSOLUTE b) => a=b
       | (RELATIVE a, RELATIVE b) => a=b
       | (RANGE (a1,a2), RANGE (b1,b2)) => (a1=b1) andalso (a2=b2)
       | (LIST l1, LIST l2) => (length l1) = (length l2)
			       andalso 
			       List.all (fn(a,b)=>a=b) (ListPair.zip (l1, l2))
       | _ => false)
    
end
