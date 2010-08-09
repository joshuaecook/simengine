structure Unique =
struct

type uniqueid = int

val seed = ref 0

fun genid () =
    !seed before seed := !seed + 1

fun unique base =
    base ^ "_" ^ (Int.toString (genid()))

end


structure UniqueTable=

struct

structure H = ListMapFn (struct
			 type ord_key = Unique.uniqueid
			 val compare = Int.compare
			 end)
					    
  type 'a table = 'a H.map
  val empty = H.empty
  fun enter (t, k, a) = H.insert(t, k, a)
  fun look(t, k) = H.find(t,k)
  fun remove(t, k) = H.remove(t, k)
  fun numItems(t) = H.numItems(t)
  fun listKeys (t) = (H.listKeys (t))
  fun listItems (t) = H.listItems (t)
  fun map(f) = H.map (f)
  fun priorityunion (tab1, tab2) = H.unionWith (fn(a,_) => a) (tab1, tab2) 

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
