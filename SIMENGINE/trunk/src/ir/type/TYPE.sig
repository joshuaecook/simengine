signature TYPE = sig
    (* See "Type Definitions" by Christopher A. Stone
     * from Advanced Topics in Types and Programming Languages. *)

    (* TODO:
     * Add Void and Vector types.
     *)

    type var
    type size = int

    type prim
    datatype rep 
      (* A concrete type representation. *)
      = Var of var
      | Arrow of rep * rep
      | Pair of rep * rep
      | Array of rep
      | Abstract of var * rep
      | Apply of rep * rep
      | Primitive of prim

    (* G |- T::K *)
    type ('G,'K) typet
    (* An specialization on types indicating well-formedness within a context of kinds. *)

    (* X::K in G *)
    type ('G,'K) typevar

    type context
    val bottom: context

    datatype kind = Proper | Operator of kind * kind | Unknown
    type proper
    (* The specialized kind of a proper type, distinguishable from a type operator. *)

    (*= Constructors =*)

    val int:
	(* G |- <>
	 * -------
	 * G |- intN::*
	 *)
	context -> size -> (context,proper) typet

    val real:
	(* G |- <>
	 * -------
	 * G |- realN::*
	 *)
	context -> size -> (context,proper) typet

    val bool:
	(* G |- <>
	 * -------
	 * G |- bool::*
	 *)
	context -> (context,proper) typet

    val array: 
	(* G |- <>
	 * -------
	 * G |- array::*->*
	 *)
	context -> (context,proper->proper) typet

    val var:
    	(* X::K in G    G |- <>
    	 * --------------------
    	 * G |- X::K
    	 *)
	(context,'K) typevar
	->
	(context,'K) typet
	 
    val poly: 
	(* G,X::K1 |- T2::K2
	 * -----------------
	 * G |- (\X::K1.T2)::(K1=>K2)
	 *)
	(('G,'K1) typevar -> ('G,'K2) typet)
	-> 
	('G,'K1->'K2) typet

    val apply: 
	(* G |- T1::(K11=>K12)    G |- T2::K11
	 * --------------------------------
	 * G |- (T1 T2)::K12
	 *)
	('G,'K11->'K12) typet * ('G,'K11) typet
	-> 
	('G,'K12) typet

    val arrow: 
	(* G |- T1::*    G |- T2::*
	 * ------------------------
	 * G |- (T1->T2)::*
	 *)
	('G,proper) typet * ('G,proper) typet 
	-> 
	('G,proper) typet

    val tuple: 
	(* G |- T1::*    G |- T2::*
	 * ------------------------
	 * G |- (T1*T2)::*
	 *)
	('G,proper) typet * ('G,proper) typet 
	-> 
	('G,proper) typet

    (*= Utilities =*)

    val equiv: ('G,'a) typet * ('G,'b) typet -> bool

    val context: ('G, 'a) typet -> context
    val kind: ('G, 'a) typet -> kind
    val rep: ('G,'a) typet -> rep
    (* Recovers the concrete representation of a type. *)

    val toString: ('G,'a) typet -> string
end
