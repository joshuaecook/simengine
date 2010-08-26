signature TYPE = sig
    (* See "Type Definitions" by Christopher A. Stone
     * from Advanced Topics in Types and Programming Languages. *)

    type var = string
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
      | Universal of var * rep
      | Primitive of prim

    (* G |- T::K *)
    type ('G,'K) typet
    (* An specialization on types indicating well-formedness within a context of kinds. *)

    type proper
    (* The kind of a proper type, distinguishable from a type operator. *)

    (*= Constructors =*)

    (* G |- int::* *)
    val int: size -> ('G,proper) typet
    (* G |- real::* *)
    val real: size -> ('G,proper) typet
    (* G |- bool::* *)
    val bool: ('G,proper) typet
    (* G |- array::(K1->K2) *)
    val array: ('G,'K1->'K2) typet

    (* FIXME: I'm not sure about this one. *)
    val var:
    	(* X::K in G    G |- <>
    	 * --------------------
    	 * G |- X::K
    	 *)
	unit -> ('G, 'K) typet
	 
    val poly: 
	(* G,X::K1 |- T2::K2
	 * -----------------
	 * G |- (\X::K1.T2)::(K1=>K2)
	 *)
	('G,'K1) typet -> ('G,'K2) typet
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

    val universal:
	(* G,X::K1 |- T2::*
	 * ----------------
	 * G |- (UX::K1.T2)::*
	 *)
	(('G,'K1) typet -> ('G,proper) typet)
	->
	('G,proper) typet

    (*= Utilities =*)

    val equiv: ('G,'a) typet * ('G,'b) typet -> bool

    val rep: ('G,'a) typet -> rep
    (* Recovers the concrete representation of a type. *)

    val toString: ('G,'a) typet -> string
end
