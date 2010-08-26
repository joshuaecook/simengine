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
val bottom: context = nil

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
	     => unknown (v::context, t)
	   | _ => raise Fail "impossible!"

fun poly f =
    let
	val id = fresh ()
	val x = unknown (bottom, Var id)
    in
	case f x
	 of y as TYPE {context, rep, kind} =>
	    operator ((Unknown,kind), context, Abstract (id, rep))
    end

fun apply (TYPE {context, rep= t, kind= Operator (_,K)}, 
	   TYPE {rep= s, ...})
    = TYPE {context= context, kind= K, rep= Apply (t, s)}
  | apply _ = raise Fail "impossible!"

fun arrow (TYPE {context, rep= t, ...}, TYPE {rep= s, ...})
    = proper (context, Arrow (t, s))

fun tuple (TYPE {context, rep= t, ...}, TYPE {rep= s, ...})
    = proper (context, Pair (s, t))

(*= Normalization =*)
structure Normalization:> sig
    (* See "Logical Relations and a Case Study in 
     * Equivalence Checking" by Karl Crary
     * from Advanced Topics in Types and Programming Languages. *)

    val equiv: ('G,'a) typet * ('G,'b) typet -> bool

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

    val base:
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
    	(('G,'K1) typevar -> ('G,'SX,'K2) teq)
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
	('G,'K) typet 
	->
	('G,'X,'K) peq

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
	('G,proper) typet -> ('G,'R,proper) peq

end = struct
datatype 'S whr = WHR of bool * rep
datatype 'S whn = WHN of bool * rep
datatype ('G,'S,'K) teq = TEQ of bool * ('G,'K) typet
datatype ('G,'p,'K) peq = PEQ of bool * ('G,'K) typet

fun beta term 
  = case term 
     of TYPE {rep= app as Apply (Abstract _, _), ...} => WHR (true, app)
      | TYPE {rep,...} => WHR (false, rep)

fun apply (WHR (p,term))
  = WHR (p,term)

fun reduce (WHR (p,term), WHN (q,_)) 
  = WHN (p andalso q, term)

fun normal (WHR (p,term))
  = WHN (not p, term)

fun base (WHN (p,term), 
	  WHN (q,_), 
	  PEQ (r, TYPE {context, kind= Proper, ...})) 
    = TEQ (p andalso q andalso r, proper (context, term))
  | base _ = raise Fail "impossible!"

fun arrow f =
    let
	val x = unknown (bottom, Var (fresh ()))
    in
	case f x
	 of TEQ (p, TYPE {context, kind= k2, rep= Apply (f, _)})
	    => TEQ (p, operator ((Unknown, k2), context, f))
	  | TEQ (_, TYPE {context, kind= k2, rep})
	    => TEQ (false, operator ((Unknown, k2), context, rep))
    end

(* FIXME needs Unit type. *)
(* val one = TEQ (true, Var (fresh ())) *)

fun var x
  = PEQ (true, x)

fun peqapply (PEQ (p, TYPE {context, kind= Operator (_,K2), rep= path}), 
	      TEQ (q, TYPE {rep= term, ...}))
  = PEQ (p andalso q, TYPE {context= context, kind= K2, rep= Apply (path, term)})
  | peqapply _ = raise Fail "impossible!"


fun const term
  = case term
     of TYPE {context, kind= Proper, rep= Primitive _} => PEQ (true, term)
      | _ => PEQ (false,term)


fun equiv (a,b) =
    let
    in
	false
    end


end
(*= end Normalization =*)


val equiv = fn _ => true

val context = fn TYPE {context,...} => context
val kind = fn TYPE {kind,...} => kind
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

