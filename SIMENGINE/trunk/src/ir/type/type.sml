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
  | Array
  | Abstract of var * rep
  | Apply of rep * rep
  | Primitive of prim

type context = var list
val bottom: context = nil

datatype kind = Proper | Operator of kind * kind | Unknown
datatype proper = datatype kind

datatype ('G,'K) typevar
  = TYPE of {context: context,
	     kind: kind,
	     rep: rep}

datatype typet = datatype typevar

fun rep (TYPE {rep,...}) = rep
fun kind (TYPE {kind,...}) = kind
fun gen (TYPE {kind,context,rep}) = TYPE {kind= kind, context= context, rep= rep}

fun isProper (TYPE {kind= Proper,...}) = true
  | isProper _ = false

fun isOperator (TYPE {kind= Operator _, ...}) = true
  | isOperator _ = false

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

fun array cxt = operator ((Proper,Proper), cxt, Array)

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

val varToString = fn s => "a"^(Int.toString (!s))
val rec kindToString 
  = fn Proper => "*"
     | Unknown => "_"
     | Operator (k1,k2) => "("^(kindToString k1)^" => "^(kindToString k2)^")"
val rec toString =
 fn Var var => varToString var
  | Arrow (a, b) => "(-> "^(toString a)^" "^(toString b)^")"
  | Pair (a, b) => "(* "^(toString a)^" "^(toString b)^")"
  | Array => "array"
  | Abstract (var, rep) => "(mu ("^(varToString var)^") "^(toString rep)^")"
  | Apply (t, s) => "("^(toString t)^" "^(toString s)^")"
  | Primitive (p,s) => p^(Int.toString s)
val toString 
  = fn TYPE {context, kind, rep} 
       => (toString rep)^"::"^(kindToString kind)

(*= Normalization =*)
structure Normalization: sig
    (* See "Logical Relations and a Case Study in 
     * Equivalence Checking" by Karl Crary
     * from Advanced Topics in Types and Programming Languages. *)

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

    val equiv: 
	(* Are two types equivalent? *)
	('G,'a) typet * ('G,'a) typet
	->
	bool

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

fun iswhr (WHR (p,_)) = p
fun iswhn (WHN (p,_)) = p
fun isteq (TEQ (p,_)) = p
fun ispeq (PEQ (p,_)) = p

(* Replaces x by substituting s in t. *)
fun subst (x,s) =
 (* [x :-> s] t *)
 fn t => raise Fail "subst"

fun beta' term 
  = case rep term
     of Apply (Abstract (x,t), s) =>
	WHR (true, subst (x,s) t)
      | t => WHR (false, t)

fun apply' (WHR (p,term))
  = WHR (p,term)

fun reduce' (WHR (p,term), WHN (q,_)) 
  = WHN (p andalso q, term)

fun normal' (WHR (p,term))
  = WHN (not p, term)

fun base' (WHN (p,term), 
	   WHN (q,_), 
	   PEQ (r, TYPE {context, kind= Proper, ...})) 
    = TEQ (p andalso q andalso r, proper (context, term))
  | base' _ = raise Fail "impossible!"

fun arrow' f =
    let
	(* Use any new variable. *)
	val id = fresh ()
	val TEQ (p, ptype) = f (unknown (bottom, Var id))
	val TYPE {context, kind, ...} = ptype
    in
	case rep ptype
	 of Apply (t, Var v) =>
	    TEQ (p andalso v = id, operator ((Unknown, kind), context, t))
	  | t =>
	    TEQ (false, operator ((Unknown, kind), context, t))
    end

(* FIXME needs Unit type. *)
(* val one = TEQ (true, Var (fresh ())) *)

fun var' x
  = case rep x
     of Var _ => PEQ (true, x)
      | _ => PEQ (false, x)

fun peqapply' (q_peq, t_teq) =
    let 
	val PEQ (p, qtype) = q_peq
	val TYPE {kind, ...} = qtype
	val TEQ (q, ttype) = t_teq
    in
	PEQ (p andalso q, apply (qtype, ttype))
    end


fun const' term
  = case term
     of TYPE {context, kind= Proper, rep= Primitive _} => PEQ (true, term)
      | _ => PEQ (false,term)



fun cross (l1, l2) = 
    let
	fun flatmap f list =
	    List.rev (List.foldl (fn (x, flat) => List.revAppend (f x, flat)) nil list)
    in
	flatmap (fn y => map (fn x => (x,y)) l1) l2
    end

fun cross3 (l1, l2, l3) = let
    fun flatmap f list =
	List.rev (List.foldl (fn (x, flat) => List.revAppend (f x, flat)) nil list)
in
    flatmap (fn z => flatmap (fn y => map (fn x => (x,y,z)) l1) l2) l3
end

fun equiv (a,b) =
    let
	val _ = print ("=? "^(toString a)^"\n   "^(toString b)^"\n")
	val teqs = nil
	val peqs = nil
	val whrs = nil
	val whns = nil

	val teqs = term_equiv (teqs,peqs,whrs,whns) (a,b)

	val peqs = path_equiv (teqs,peqs,whrs,whns) (a,b)

	val whrs = wh_reduction (teqs,peqs,whrs,whns) a
	val whrs = wh_reduction (teqs,peqs,whrs,whns) b

	val whns = wh_normal (teqs,peqs,whrs,whns) a
	val whns = wh_normal (teqs,peqs,whrs,whns) b
    in
	true
    end

and term_equiv (teqs,peqs,whrs,whns) (a,b) =
    let
	val teqs
	  (* 3D crossproduct mapping of normalization judgements squared and path equivalance judgements giving base path equivalance judgements. *)
	  = foldl
		(fn (sn_tn_qq,c) => base' sn_tn_qq :: c)
		teqs
		(cross3 (whns,whns,peqs))

	fun arrow_teq (TEQ (p,t)) =
	 fn a => TEQ (p, apply (t, var a))

	val teqs
	  (* Arrow term equivalance. *)
	  = foldl
		(fn (tx_q, c) => arrow' (arrow_teq tx_q) :: c)
		teqs
		teqs
    in
	teqs
    end

and path_equiv (teqs,peqs,whrs,whns) (p,q) =
    let
	(* Variable self-equivalance judgements. *)
	val peqs = var' p :: var' q :: peqs

	val peqs
	  (* Cross product mapping of path equivalence judgements and term equivalance judgements giving application path equivalance judgements. *)
	  = foldl
		(fn (pq_tq,c) => peqapply' pq_tq :: c)
		peqs
		(cross (peqs,teqs))

	(* Constant self-equivalence judgements. *)
	val peqs = const' p :: const' q :: peqs
    in
	peqs
    end

and wh_normal (teqs,peqs,whrs,whns) term =
    let
	val whns
	  (* Cross-product mapping of reduction and normalization judgements for further reduction judgements. *)
	  = foldl
		(fn ((sq,tq),c) => reduce' (sq,tq) :: c)
		whns
		(cross (whrs,whns))

	val whns 
	  (* Mapping of unreduceable judgements to normalization judgements. *)
	  = foldl
		(fn (tr,c) => normal' tr :: c)
		whns
		whrs
    in
	whns
    end

and wh_reduction (teqs,peqs,whrs,whns) term = 
    let
	val whrs 
	  (* Beta reduction judgement. *)
	  = beta' term :: whrs

	val whrs
	  (* Mapping of reduction judgements to application reduction judgements. *)
	  = foldl
		(fn (tr,c) => apply' tr :: c)
		whrs
		whrs
    in
	whrs
    end


val beta = beta'
val apply = apply'
val reduce = reduce'
val normal = normal'
val base = base'
val arrow = arrow'
val var = var'
val peqapply = peqapply'
val const = const'

end
(*= end Normalization =*)


val equiv = 
 Normalization.equiv


end

