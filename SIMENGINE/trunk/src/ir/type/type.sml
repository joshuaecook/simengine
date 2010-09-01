structure Type:> TYPE = struct

type var = int ref
(* Variables are equivalent iff their contents are pointer equivalent.
 * The int inside the ref is only used for printing a name. *)

type size = int
type prim = string * size

fun primitiveSize (_,x) = x
fun primitiveName (x,_) = x

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

datatype kind = Proper | Operator of kind * kind | Product of kind * kind | Unknown
datatype proper = datatype kind

datatype ('G,'K) typevar
  = TYPE of {context: context,
	     kind: kind,
	     rep: rep}

type ('G,'K1,'K2) typecon = ('G,'K1->'K2) typevar
datatype typet = datatype typevar

type t = (context,kind) typet
type proper_t = (context,proper) typet

fun rep (TYPE {rep,...}) = rep
fun kind (TYPE {kind,...}) = kind
fun gen (TYPE {kind,context,rep}) = TYPE {kind= kind, context= context, rep= rep}
fun gencon (TYPE {kind,context,rep}) = TYPE {kind= kind, context= context, rep= rep}

fun isProper (TYPE {kind= Proper,...}) = true
  | isProper _ = false

fun isConstructor (TYPE {kind= Operator _, ...}) = true
  | isConstructor _ = false


val varName = fn s => "a"^(Int.toString (!s))
val rec kindToString 
  = fn Proper => "*"
     | Unknown => "_"
     | Product (k1,k2) => "("^(kindToString k1)^" x "^(kindToString k2)^")"
     | Operator (k1,k2) => "("^(kindToString k1)^" => "^(kindToString k2)^")"
val rec toString =
 fn Var var => varName var
  | Arrow (a, b) => "("^(toString a)^"->"^(toString b)^")"
  | Pair (a, b) => "("^(toString a)^"*"^(toString b)^")"
  | Array => "array"
  | Abstract (var, rep) => "(fn "^(varName var)^" => "^(toString rep)^")"
  | Apply (t, s) => "("^(toString t)^" "^(toString s)^")"
  | Primitive (p,s) => p^(Int.toString s)
val toString 
  = fn TYPE {context, kind, rep} 
       => (toString rep)^"::"^(kindToString kind)


fun isfree (var, term)
  = case term
     of Var v => v = var
      | Arrow (a,b) => isfree (var,a) andalso isfree (var,b)
      | Pair (a,b) => isfree (var,a) andalso isfree (var,b)
      | Array => true
      | Abstract (v,b) => if v = var then false else isfree (var,b)
      | Apply (a,b) => isfree (var,a) andalso isfree (var,b)
      | Primitive _ => true


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

fun int n
  = proper (bottom, Primitive ("int", n))

fun real n 
  = proper (bottom, Primitive ("real", n))

val string: (context,proper) typet
  = proper (bottom, Primitive ("string", 0))

val bool: (context,proper) typet
  = proper (bottom, Primitive ("bool", 1))

val array: (context,proper->proper) typet
  = operator ((Proper,Proper), bottom, Array)

val var = fn TYPE {context, rep= t as Var v, ...} 
	     => unknown (v::context, t)
	   | _ => raise Fail "impossible!"

fun var v = v

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

  | apply (TYPE {context, rep= t, ...}, TYPE {rep= s, ...})
    = TYPE {context= context, kind= Unknown, rep= Apply (t, s)}

fun arrow (TYPE {context, rep= t, ...}, TYPE {rep= s, ...})
    = proper (context, Arrow (t, s))

fun tuple (TYPE {context, rep= t, ...}, TYPE {rep= s, ...})
    = proper (context, Pair (s, t))


local
    open Layout
in
val varToLayout = fn s => seq [str "a", str (Int.toString (!s))]
val rec kindToLayout 
  = fn Proper => str "*"
     | Unknown => str "_"
     | Operator (k1,k2) => paren (seq [kindToLayout k1,
				       str " => ",
				       kindToLayout k2])
     | Product (k1,k2) => paren (seq [kindToLayout k1,
				      str " * ",
				      kindToLayout k2])

val rec toLayout =
 fn Var var => varToLayout var
  | Arrow (a, b) => paren (seq [toLayout a,
				str " -> ",
				toLayout b])
  | Pair (a, b) => paren (seq [toLayout a,
			       str " * ",
			       toLayout b])
  | Array => str "array"
  | Abstract (var, rep) => paren (seq [str "mu ",
				       paren (varToLayout var),
				       str " ",
				       toLayout rep])
  | Apply (t, s) => paren (seq [toLayout s, str " ", toLayout t])
  | Primitive (p,s) => seq [str p, str (Int.toString s)]
val toLayout
  = fn TYPE {context, kind, rep} 
       => seq [toLayout rep,
	       str "::",
	       kindToLayout kind]
and toSML 
  = fn TYPE {context, kind, rep}
       => toLayout rep

end

structure Reduction: sig
    type 'T poly
    type ('T1,'T2) apply
    type ('T1,'T2) beta
    type 'T eta

    (* (S red T) *)
    type 'T typered

    val reflect:
	(* (T red T) *)
	rep -> rep typered

    val poly:
	(* (S2 red T2)
	 * ----------
	 * ((\X::K1.S2) red (\X::K1.T2))
	 *)
	'T2 typered
	->
	'T2 poly typered

    val apply:
	(* (S1 red T1)    (S2 red T2)
	 * -------------
	 * ((S1 S2) red (T1 T2))
	 *)
	'T1 typered * 'T2 typered
	->
	('T1, 'T2) apply typered

    val beta:
	(* (S1 red T1)    (S2 red T2)
	 * ---------------
	 * (((\X::K.S1) S2) red ([X :-> T2] T1))
	 *)
	'T1 typered * 'T2 typered
	->
	('T1,'T2) beta typered

    val eta:
	(* (S red T)    X not free in S
	 * -----------------
	 * ((\X::K.S X) red T)
	 *)
	'T typered
	->
	'T eta typered

    val eval:
	(* (S red T)
	 * --------
	 * T
	 *)
	'T typered -> rep
end = struct

datatype class 
  = Refl of rep
  | Abs of class
  | App of class * class
  | Beta of class * class
  | Eta of class

type 'T poly = unit
type ('T1,'T2) apply = unit
type ('T1,'T2) beta = unit
type 'T1 eta = unit

type 'T typered = class

fun reflect t = Refl t

fun poly red = Abs red

fun apply (red1,red2)
  = App (red1, red2)

fun beta (red1, red2)
  = Beta (red1, red2)

fun eta red = Eta red


fun beta_redex (subst, Abstract (var,body)) = 
    let
	fun redex term
	  = case term
	     of Array => term
	      | Primitive _ => term
	      | Arrow (s1,s2) => Arrow (redex s1, redex s2)
	      | Pair (s1,s2) => Pair (redex s1, redex s2)
	      | Apply (s1,s2) => Apply (redex s1,redex s2)

	      | Var v
		=> if v = var then subst else term

	      | Abstract (v,s2)
		=> if v = var then term else Abstract (v,redex s2)
    in
	redex body
    end
  | beta_redex _ = raise Fail "impossible!"

val rec eval =
 fn Refl t => t
  | Abs r2 => eval r2
  | App (r1,r2) => Apply (eval r1, eval r2)
  | Beta (r1,r2) => beta_redex (eval r2, eval r1)
  | Eta r2 => eval r2


end








fun equiv (a,b) = 
    case (normal a, normal b)
     of (Var va, Var vb) => va = vb
      | (Primitive pa, Primitive pb) => pa = pb
      | (Array, Array) => true

      | (Arrow (s1,s2), Arrow(t1,t2))
	=> equiv (s1,t1) andalso equiv (s2,t2)

      | (Pair (s1,s2), Pair (t1,t2))
	=> equiv (s1,t1) andalso equiv (s2,t2)

      | (Abstract (va,s2), Abstract (vb,t2))
	=> va = vb andalso equiv (s2,t2)

      | (Apply (s1,s2), Apply (t1,t2))
	=> equiv (s1,t1) andalso equiv (s2,t2)

      | _ => false

and normal term
  = let open Reduction 
    in
	eval (case term
	       of Var _ => reflect term
		| Primitive _ => reflect term
		| Array => reflect term

		| Arrow (s1,s2) 
		  => reflect (Arrow (normal s1, normal s2))

		| Pair (s1,s2)
		  => reflect (Pair (normal s1, normal s2))

		| Apply (s1, s2) 
		  => (case normal s1
		       of t1 as Abstract (var, _)
			  => beta (reflect t1, reflect (normal s2))
			| t1 => reflect (Apply (t1, normal s2)))

		| Abstract (var, s11)
		  => (case normal s11
		       of t2 as Apply (s21, s22)
			  => if equiv (Var var, s22) andalso isfree (var, s21)
			     then eta (reflect (normal s21))
			     else reflect (Abstract (var, normal s22))
		    | t2 => reflect (Abstract (var, t2))))
    end


val equiv = fn (a,b) => equiv (rep a, rep b)
val normal = fn a => normal (rep a)

end




