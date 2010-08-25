structure Type:> TYPE = struct

type var = string
type size = int
type prim = var * size

datatype rep 
  = Var of var
  | Arrow of rep * rep
  | Pair of rep * rep
  | Array of rep
  | Abstract of var * rep
  | Apply of rep * rep
  | Universal of var * rep
  | Primitive of prim

type 'a typet = rep

val var = Var
val tuple = Pair
val arrow = Arrow
fun apply t s = Apply (t, s)

local
    val n = ref 0
    fun fresh () = 
	let val n' = !n in 
	    ("a" ^ (Int.toString n')) before n := (1 + n') 
	end
in
fun poly f =
    let val x = fresh () in
	Abstract (x, f (var x))
    end
end

val array = poly Array

val int = fn n => Primitive ("int",n)
val real = fn n => Primitive ("real",n)
val bool = Primitive ("bool",1)


val rep = fn x => x

val rec toString =
 fn Var var => var
  | Arrow (a, b) => "(-> "^(toString a)^" "^(toString b)^")"
  | Pair (a, b) => "(* "^(toString a)^" "^(toString b)^")"
  | Array rep => "(array "^(toString rep)^")"
  | Abstract (var, rep) => "(mu ("^var^") "^(toString rep)^")"
  | Apply (t, s) => "("^(toString s)^" "^(toString t)^")"
  | Universal (var, rep) => "(all ("^var^") "^(toString rep)^")"
  | Primitive (p,s) => "("^p^" "^(Int.toString s)^")"

end
