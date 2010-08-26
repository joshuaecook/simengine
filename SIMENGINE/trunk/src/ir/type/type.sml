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
  | Primitive of prim

type context = var list
val base: context = nil

datatype kind = Proper | Operator of kind * kind | Unknown
datatype proper = datatype kind

datatype ('G,'K) typet 
  = TYPE of {context: context,
	     kind: kind,
	     rep: rep}

datatype typevar = datatype typet

local
    val n = ref 0
in
fun fresh () = 
    (* Creates a new identifier. *)
    let val s = ref (1+(!n)) in s before n := !s end
end

fun proper (cxt,t) = TYPE {context= cxt, kind= Proper, rep= t}
fun operator (kk,cxt,t) = TYPE {context= cxt, kind= Operator kk, rep= t}
fun unknown (cxt,t) = TYPE {context= cxt, kind= Unknown, rep= t}

fun int cxt 
  = fn n => proper (cxt, Primitive ("int", n))

fun real cxt 
  = fn n => proper (cxt, Primitive ("real", n))

fun bool cxt = proper (cxt, Primitive ("bool", 1))

fun array cxt = operator ((Proper,Proper), cxt, Array (Var (fresh ())))

val var = fn TYPE {context, rep= t as Var v, ...} 
	     => proper (v::context, t)
	   | _ => raise Fail "impossible!"

fun poly f =
    let
	val id = fresh ()
	val x = unknown (base, Var id)
    in
	case f x
	 of y as TYPE {context, rep, kind} =>
	    operator ((Unknown,kind), context, Abstract (id, rep))
    end

(* FIXME: kind? *)
fun apply (TYPE {context, rep= t, kind= Operator (_,K)}, 
	   TYPE {rep= s, ...})
    = TYPE {context= context, kind= K, rep= Apply (t, s)}
  | apply _ = raise Fail "impossible!"

fun arrow (TYPE {context, rep= t, ...}, TYPE {rep= s, ...})
    = proper (context, Arrow (t, s))

fun tuple (TYPE {context, rep= t, ...}, TYPE {rep= s, ...})
    = proper (context, Pair (s, t))

(* (\*= Reduction =*\) *)
(* structure Reduction: sig *)
(*     (\* T red S *\) *)
(*     type red *)

(*     val reflectivity: *)
(* 	(\* T red T *\) *)
(* 	red *)

(*     val poly: *)
(* 	(\* S2 red T2 *)
(* 	 * --------- *)
(* 	 * (\X::K1.S2) red (\X::K1.T2) *)
(* 	 *\) *)
(* 	red *)
(* 	-> *)
(* 	red *)

(*     val apply: *)
(* 	(\* S1 red T1    S2 red T2 *)
(* 	 * ---------------------- *)
(* 	 * (S1 S2) red (T1 T2) *)
(* 	 *\) *)
(* 	red * red *)
(* 	-> *)
(* 	red *)

(*     val beta: *)
(* 	(\* S1 red T1    S2 red T2 *)
(* 	 * ---------------------- *)
(* 	 * (S1 S2) red (T1 T2) *)
(* 	 *\) *)
(* 	red * red *)
(* 	-> *)
(* 	red *)

(*     val eta: *)
(* 	(\* S red T    fresh X *)
(* 	 * ------------------ *)
(* 	 * (\X::K.S X) red T *)
(* 	 *\) *)
(* 	red *)
(* 	-> *)
(* 	red *)
(* end = struct *)
(* end *)
(* (\*= end Reduction =*\) *)

(*
(*= Normalization =*)
structure Normalization:> sig
    (* S ~> T *)
    type 'S whr
    (* Weak head reduction. *)

    (* S v T *)
    type 'S whn
    (* Weak head normalization. *)

    (* G |- (S teq T)::K *)
    type ('G, 'S, 'K) teq
    (* Algorithmic term equivalence. *)

    (* G |- (p peq q)::K *)
    type ('G, 'p, 'K) peq
    (* Algorithmic path equivalence. *)

    val beta:
	(* ((\X::K12.T12) T2) ~> ([X :-> T2] T12) *)
	('G,'K) typet -> 'K whr

    val apply:
	(* T1 ~> T'1
	 * ---------
	 * (T1 T2) ~> (T'1 T2)
	 *)
	'T1 whr 
	-> 
	'T1T2 whr

    val reduce:
	(* S ~> T    T v U
	 * ---------------
	 * S v U
	 *)
	'S whr * 'T whn
	->
	'S whn

    val normal:
	(* T !~>
	 * -----
	 * T v T
	 *)
	'T whr
	->
	'T whn

    val proper:
	(* S v p    T v q    G |- (p peq q)::*
	 * -----------------------------------
	 * G |- (S teq T)::*
	 *)
	'S whn * 'T whn * ('G,'p,proper) peq
	->
	('G,'S,proper) teq

    val arrow:
	(* G,X::K1 |- ((S X) teq (T X))::K2
	 * --------------------------------
	 * G |- (S teq T)::(K1 => K2)
	 *)
	(('G,'K1) typet -> ('G,'SX,'K2) teq)
	->
	('G,'S,'K1->'K2) teq

    (* val one: *)
    (* 	(\* G |- (S teq T)::Unit *\) *)
    (* 	('G, 'S, unit) teq *)

    val var:
	(* X::K in G    G |- <>
	 * --------------------
	 * G |- (X peq X)::K
	 *)
	(unit,'K) typet 
	->
	(unit,'X,'K) peq

    val peqapply:
	(* G |- (p peq q)::(K1=>K2)    G |- (S teq T)::K1
	 * ----------------------------------------------
	 * G |- ((p S) peq (q T))::K2
	 *)
	('G,'p,'K1->'K2) peq * ('G,'S,'K1) teq
	->
	('G,'pS,'K2) peq

    val const:
	(* G |- (R peq R)::proper *)
	(unit,proper) typet -> ('G,'R,proper) peq

end = struct
datatype 'S whr = WHR of bool * rep
datatype 'S whn = WHN of bool * rep
datatype ('G,'S,'K) teq = TEQ of bool * rep
datatype ('G,'p,'K) peq = PEQ of bool * rep

fun beta term 
  = case term 
     of Apply (Abstract _, _) => WHR (true, term)
      | _ => WHR (false, term)

fun apply (WHR (p,term))
  = WHR (p,term)

fun reduce (WHR (p,term), WHN (q,_)) 
  = WHN (p andalso q, term)

fun normal (WHR (p,term))
  = WHN (not p, term)

fun proper (WHN (p,term), WHN (q,_), PEQ (r,_)) 
  = TEQ (p andalso q andalso r, term)

fun arrow f =
    case f (Var (fresh()))
     of TEQ (p, Apply (term, _)) => TEQ (p, term)
      | TEQ (_, term) => TEQ (false, term)

(* FIXME needs Unit type. *)
(* val one = TEQ (true, Var (fresh ())) *)

fun var _ 
  = PEQ (true, Var (fresh ()))

fun peqapply (PEQ (p,path), TEQ (q,term))
  = PEQ (p andalso q, Apply (path, term))

fun const term 
  = case term
     of Primitive _ => PEQ (true,term)
      | _ => PEQ (false,term)


fun equiv (a,b) =
    let
    in
	false
    end


end
(*= end Normalization =*)
*)

val equiv = fn _ => true

(*
(*= Equivalency =*)
structure Equivalence: sig
    (* G |- (T eq S)::K *)
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

    val extensional:
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

and poly _ (p, x as Abstract _) = (p,x)
  | poly _ (_,x) = (false, x)

and apply ((p,x),(q,_)) = (p andalso q, x)

and beta _ = raise Fail "TODO beta equiv"
and extensional _ = raise Fail "TODO extensional equiv"
end
(*= end Equivalency =*)
*)



val rep = fn TYPE {rep,...} => rep

val varToString = fn s => "a"^(Int.toString (!s))
val rec kindToString 
  = fn Proper => "*"
     | Unknown => "_"
     | Operator (k1,k2) => "(-> "^(kindToString k1)^" "^(kindToString k2)^")"

val rec toString =
 fn Var var => varToString var
  | Arrow (a, b) => "(-> "^(toString a)^" "^(toString b)^")"
  | Pair (a, b) => "(* "^(toString a)^" "^(toString b)^")"
  | Array rep => "(array "^(toString rep)^")"
  | Abstract (var, rep) => "(mu ("^(varToString var)^") "^(toString rep)^")"
  | Apply (t, s) => "("^(toString t)^" "^(toString s)^")"
  | Primitive (p,s) => p^(Int.toString s)

val toString 
  = fn TYPE {context, kind, rep} 
       => (toString rep)^"::"^(kindToString kind)

end

