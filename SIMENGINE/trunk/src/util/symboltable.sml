(* Copyright Simatra - 2006, 2010 
 * symboltable.sml - Symbol Table data structure
 *
 *   This data structure is useful for mapping symbols to 'a
 *
 *   Note: taken partly from free domain Andrew Appel's code
 *)
structure SymbolTable=

struct

structure H = ListMapFn (struct
			 type ord_key = Symbol.symbol
			 val compare = Symbol.compare
			 end)
					    
  type 'a table = 'a H.map
  val empty = H.empty
  fun enter (t, k, a) = H.insert(t, k, a)
  fun look(t, k) = H.find(t,k)
  fun remove(t, k) = H.remove(t, k)
  fun numItems(t) = H.numItems(t)
  fun listKeys (t) = (H.listKeys (t))
  fun listItems (t) = H.listItems (t)
  fun listItemsi (t) = H.listItemsi (t)
  fun map(f) = H.map (f)
  fun mapi(f) = H.mapi (f)
  fun app(f) = H.app (f)
  fun appi(f) = H.appi (f)
  fun priorityunion (tab1, tab2) = H.unionWith (#1) (tab1, tab2) 

  fun null t = 0 = numItems t
  val foldl = H.foldl
  val foldli = H.foldli
  val filteri = H.filteri

  fun addCount t = ListPair.zip (listItems t, List.tabulate (numItems t, fn(x)=>x))
  (* val mapToTable = (symbol -> a') -> symbol list -> a' table *)
  fun mapToTable fcn l = List.foldl
			     (fn((k, v), t) => enter (t, k, v))
			     empty
			     (ListPair.zipEq (l, (List.map fcn l)))

(*
  type 'a table = (string * 'a) list
  val empty = nil
  fun enter (t,k,a) = (k,a) :: t
  fun look (t,k) = case (List.find (fn(k',a') => k = k') t) of
		       SOME (k,a) => SOME a
		     | NONE => NONE
  fun remove (t,k) = List.filter (fn(k',a') => k <> k') t
  fun numItems (t) = length t
  fun listKeys (t) = map (fn(k,a) => k) t
  fun listItems (t) = map (fn(k,a) => a) t
  val map = map
  fun priorityunion (tab1, tab2) = tab1 @ tab2*)
end

(*
structure Key : ORD_KEY
type 'a map
val empty : 'a map 
val insert : ('a map * Key.ord_key * 'a) -> 'a map 
val insert' : ((Key.ord_key * 'a) * 'a map) -> 'a map 
val find : ('a map * Key.ord_key) -> 'a option 
val remove : ('a map * Key.ord_key) -> ('a map * 'a) 
val numItems : 'a map -> int 
val listItems : 'a map -> 'a list 
val listItemsi : 'a map -> (Key.ord_key * 'a) list 
val collate : (('a * 'a) -> order) -> ('a map * 'a map) -> order 
val unionWith : (('a * 'a) -> 'a) -> ('a map * 'a map) -> 'a map 
val unionWithi : ((Key.ord_key * 'a * 'a) -> 'a) -> ('a map * 'a map) -> 'a map 
val intersectWith : (('a * 'b) -> 'c) -> ('a map * 'b map) -> 'c map 
val intersectWithi : ((Key.ord_key * 'a * 'b) -> 'c) -> ('a map * 'b map) -> 'c map 
val app : ('a -> unit) -> 'a map -> unit 
val appi : ((Key.ord_key * 'a) -> unit) -> 'a map -> unit 
val map : ('a -> 'b) -> 'a map -> 'b map 
val mapi : ((Key.ord_key * 'a) -> 'b) -> 'a map -> 'b map 
val foldl : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b 
val foldli : ((Key.ord_key * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b 
val foldr : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b 
val foldri : ((Key.ord_key * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b 
val filter : ('a -> bool) -> 'a map -> 'a map 
val filteri : ((Key.ord_key * 'a) -> bool) -> 'a map -> 'a map 
val mapPartial : ('a -> 'b option) -> 'a map -> 'b map 
val mapPartiali : ((Key.ord_key * 'a) -> 'b option) -> 'a map -> 'b map 
*)
