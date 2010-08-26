structure Type:> TYPE = struct

type var = int ref
(* Variables are equivalent iff their contents are pointer equivalent.
 * The int inside the ref is only used for printing a name. *)

type size = int
type prim = string * size

datatype rep 
  = Var of var
  | Arrow of rep * rep
  | Pair of rep * rep
  | Array of rep
  | Abstract of var * rep
  | Apply of rep * rep
  | Universal of var * rep
  | Primitive of prim

type ('G,'a) typet = rep
type proper = unit

local
    val n = ref 0
in
fun fresh () = 
    (* Creates a new identifier. *)
    let val s = ref (1+(!n)) in s before n := !s end
end

val var = fn x => x
val tuple = Pair
val arrow = Arrow
val apply = Apply
val array = Array (Var (fresh ()))

fun poly f =
    let val x = fresh () in
	Abstract (x, f (Var x))
    end

fun universal f =
    let val x = fresh () in
	Universal (x, f (Var x))
    end


val int = fn n => Primitive ("int",n)
val real = fn n => Primitive ("real",n)
val bool = Primitive ("bool",1)

(*= Equivalency =*)
structure Equivalence: sig
    (* G |- (T eq T)::K *)
    type ('G,'K) eq

    val equiv: ('G,'K1) typet * ('G,'K2) typet -> bool

    val reflectivity:
	(* G |- T::K
	 * ---------
	 * G |- (T eq T)::K
	 *)
	('G,'K) typet
	->
	('G,'K) eq
	
    val symmetry: 
	(* G |- (T eq S)::K
	 * --------------
	 * G |- (S eq T)::K
	 *) 
	('G,'K) eq
	-> 
	('G,'K) eq

    val transitivity: 
	(* G |- (S eq U)::K    G |- (U eq T)::K
	 * --------------------------------
	 * G |- (S eq T)::K
	 *)
	('G,'K) eq * ('G,'K) eq
	->
	('G,'K) eq

    val arrow:
	(* G |- (S1 eq T1)::*    G |- (S2 eq T2)::*
	 * ------------------------------------
	 * G |- ((S1->S2) eq (T1->T2))::*
	 *)
	('G,proper) eq * ('G,proper) eq
	->
	('G,proper) eq

    val tuple:
	(* G |- (S1 eq T1)::*    G |- (S2 eq T2)::*
	 * ------------------------------------
	 * G |- ((S1*S2) eq (T1*T2))::*
	 *)
	('G,proper) eq * ('G,proper) eq
	->
	('G,proper) eq

    val universal:
	(* G,X::K1 |- (S2 eq T2)::*
	 * ------------------------
	 * G |- ((UX::K1.S2) eq (UX::K1.T2))::*
	 *)
	('G,'K1) typet -> ('G,proper) eq
	->
	('G,proper) eq

    val poly:
	(* G,X::K1 |- (S2 eq T2)::K2
	 * -----------------------
	 * G |- ((\X::K1.S2) eq (\X::K1.T2))::(K1=>K2) 
	 *)
	('G,'K1) typet -> ('G,'K2) eq
	->
	('G,'K1->'K2) eq

    val apply:
	(* G |- (S1 eq T1)::(K11=>K12)
	 * G |- (S2 eq T2)::K11
	 * -----------------------
	 * G |- ((S1 S2) eq (T1 T2))::K12
	 *)
	('G,'K11->'K12) eq *
	('G,'K1) eq
	->
	('G,'K12) eq

    val beta:
	(* G,X::K11 |- (S12 eq T12)::K12
	 * G |- (S2 eq T2)::K11
	 * ---------------------------
	 * G |- (((\X:K11.S12) S2) eq ([X->T2] T12))::K12
	 *)
	(('G,'K11) typet -> ('G,'K12) eq) *
	('G,'K11) eq
	->
	('G,'K12) eq

    val nu:
	(* G,X::K1 |- ((S X) eq (T X))::K2
	 * -----------------------------
	 * G |- (S eq T)::K1->K2
	 *)
	('G,'K1) typet -> ('G,'K2) eq
	->
	('G,proper) eq
end = struct
type ('G,'K) eq = bool * ('G,'K) typet

fun equiv (x,y) = 
    let 
	val is = fn (p,_) => p
	val snd = fn (_,qq) => qq

	val check 
 	  = case x
	     of Arrow _ => is o arrow
	      | Pair _ => is o tuple
	      | Universal (v,_) => is o (universal (Var v)) o snd
	      | Abstract (v,_) => is o (poly (Var v)) o snd
	      | Apply _ => is o apply

	      | Array _ => raise Fail "TODO array equiv"

	      | Var vx => (fn _ => false)
	      | Primitive tx 
		=> (fn _ => (case y of Primitive ty => tx = ty | _ => false))
    in
	check (reflectivity x, reflectivity y)
    end

and reflectivity x = (true, x)
and symmetry (p,x) = (p,x)
and transitivity ((p,x),(q,_)) = (p andalso q, x)

and arrow ((p, x as Arrow (al1,ar1)), (q, Arrow (al2,ar2))) = 
    (p andalso q andalso equiv (al1,al2) andalso equiv (ar1,ar2), x)
  | arrow ((_,x),_) = (false, x)

and tuple ((p, x as Pair (tl1,tr1)), (q, Pair (tl2,tr2))) =
    (p andalso q andalso equiv (tl1,tl2) andalso equiv (tr1,tr2), x)
  | tuple ((_,x),_) = (false, x)

and universal _ (p, x as Universal _) = (p,x)
  | universal _ (_,x) = (false, x)

and poly _ (p, x as Abstract _) = (p,x)
  | poly _ (_,x) = (false, x)

and apply ((p,x),(q,_)) = (p andalso q, x)

and beta _ = raise Fail "TODO beta equiv"
and nu _ = raise Fail "TODO nu equiv"
end

val equiv = Equivalence.equiv

val rep = fn x => x

val varToString = fn s => "a"^(Int.toString (!s))

val rec toString =
 fn Var var => varToString var
  | Arrow (a, b) => "(-> "^(toString a)^" "^(toString b)^")"
  | Pair (a, b) => "(* "^(toString a)^" "^(toString b)^")"
  | Array rep => "(array "^(toString rep)^")"
  | Abstract (var, rep) => "(mu ("^(varToString var)^") "^(toString rep)^")"
  | Apply (t, s) => "("^(toString t)^" "^(toString s)^")"
  | Universal (var, rep) => "(all ("^(varToString var)^") "^(toString rep)^")"
  | Primitive (p,s) => "("^p^" "^(Int.toString s)^")"

end

