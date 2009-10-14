(* Taken from Andrew Appel's public domain source code of the Tiger
 * Compiler listed in _Modern Compiler Implementation in ML_ *)

(**)

signature TABLE = 
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
end


functor IntMapTable (type key
		     val getInt: key -> int) : TABLE =
struct
  type key=key
  type 'a table = 'a IntListMap.map
  val empty = IntListMap.empty
  fun enter(t,k,a) = IntListMap.insert(t,getInt k,a)
  fun look(t,k) = IntListMap.find(t,getInt k)
end


signature SYMBOL =
sig
  eqtype symbol
  val symbol : string -> symbol
  val name : symbol -> string
  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val look  : 'a table * symbol -> 'a option
  val compare : symbol * symbol -> order
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string,int) H.hash_table = 
		H.mkTable(HashString.hashString, op = ) (sizeHint,Symbol)
  
  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end

  fun name(s,n) = s
  fun compare ((_,i1),(_,i2)) = Int.compare (i1, i2)

  structure Table = IntMapTable(type key = symbol
				fun getInt(s,n) = n)

  type 'a table= 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val look = Table.look

end

(* for API see: http://www.smlnj.org/doc/smlnj-lib/Manual/binary-set-fn.html *)
(*Interface for ORD_SET:

structure Key : ORD_KEY
type item = Key.ord_key 
type set
val empty : set 
val singleton : item -> set 
val add : (set * item) -> set 
val add' : (item * set) -> set 
val addList : (set * item list) -> set 
val delete : (set * item) -> set 
val member : (set * item) -> bool 
val isEmpty : set -> bool 
val equal : (set * set) -> bool 
val compare : (set * set) -> order 
val isSubset : (set * set) -> bool 
val numItems : set -> int 
val listItems : set -> item list 
val union : (set * set) -> set 
val intersection : (set * set) -> set 
val difference : (set * set) -> set 
val map : (item -> item) -> set -> set 
val app : (item -> unit) -> set -> unit 
val foldl : ((item * 'b) -> 'b) -> 'b -> set -> 'b 
val foldr : ((item * 'b) -> 'b) -> 'b -> set -> 'b 
val filter : (item -> bool) -> set -> set 
val exists : (item -> bool) -> set -> bool 
val find : (item -> bool) -> set -> item option *)

structure SymbolSet =
struct
structure SSet =
BinarySetFn(type ord_key = Symbol.symbol
val compare = Symbol.compare)

open SSet

fun fromList alist = addList(empty, alist)
fun toStrList symset = 
    let 
	val symbols = listItems symset
    in
	List.map Symbol.name symbols
    end
fun toStr symset =
    "{" ^ (String.concatWith ", " (toStrList symset)) ^ "}"

(* val unionList : set list -> set *)
fun unionList [] = empty
  | unionList [set1] = set1
  | unionList (set1::rest) = List.foldl union set1 rest

(* val flatmap : (a' -> set) -> a' list -> set *)
fun flatmap mapfun list = 
    unionList (List.map mapfun list)

end
