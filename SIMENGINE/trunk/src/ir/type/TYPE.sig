signature TYPE = sig
    (* See "Type Definitions" by Christopher A. Stone
     * from Advanced Topics in Types and Programming Languages. 
     * 
     * TYPE is a simply-typed, first-order language for describing
     * typed information. "Kinds" are the types of the language and
     * "types" are the terms.
     *)

    exception IllTyped

    eqtype var
    type bits = Size.bits
    eqtype prim

    type field = string

    datatype rep 
      (* The terms of the TYPE language. *)
      = Int of bits
      | Real of bits
      | String
      | Bool
      | Var of var
      (* A type variable.*)
      | Top
      (* The "top" of all subtype relations. *)
      | Bottom
      (* The "bottom" of all subtype relations. *)
      | Arrow of rep * rep
      (* The type of a function. *)
      | Record of (field * rep) vector
      (* The type of linear, keyed objects. *)
      | Abstract of var * rep
      (* A polymorphic type abstraction. *)
      | Apply of rep * rep
      (* Type application. *)
      | Universal of (var * rep) * rep
      | Existential of (var * rep) * rep
      | Array
      | Vector
      | Source
      | Sink
      | Void

    (* G |- T::K *)
    type ('G,'K) typet
    (* A specialization on types indicating well-formedness within a context. *)

    (* X::K in G *)
    type ('G,'K) typevar
    (* A specialization on types indicating a variable. *)

    type context
    val bottom: context

    type kind
    (* The kind of a type. *)

    type proper
    (* The specialized kind of a proper type, distinguishable from a type constructor. *)

    type 'a cell
    (* The specialized kind of a reference cell. *)

    type t = (context,kind) typet
    (* The general type. *)
    type proper_t = (context,proper) typet
    (* The proper type. *)

    (*= Constants and Constructors (Kinding Rules) =*)

    (* Creates an integer type identifier with an arbitrary bit length. *)
    val int:
	(* G |- <>
	 * -------
	 * G |- intN::*
	 *)
	bits -> (context,proper) typet

    (* Creates a floating-point type identifier with an arbitrary bit length. *)
    val real:
	(* G |- <>
	 * -------
	 * G |- realN::*
	 *)
	bits -> (context,proper) typet

    (* The 1-bit boolean type identifier. *)
    val bool:
	(* G |- <>
	 * -------
	 * G |- bool::*
	 *)
	(context,proper) typet

    (* A variable length string type. *)
    val string:
	(* G |- <>
	 * -------
	 * G |- string::*
	 *)
	(context,proper) typet

    (* The sequence type constructor. *)
    val array: 
	(* G |- <>
	 * -------
	 * G |- array::*=>*
	 *)
	(context,proper->proper) typet

    val void:
	(* G |- <>
	 * -------
	 * G |- void::*
	 *)
	(context,proper) typet

    (* Promotes a type variable to a type identifier. 
     * Typically used in the body of a poly constructor. *)
    val var:
    	(* X::K in G    G |- <>
    	 * --------------------
    	 * G |- X::K
    	 *)
	(context,'K) typevar
	->
	(context,'K) typet
	
    (* Constructs a polymorphic type constructor defined
     * by a given function.
     * 
     * For example, a pair of the same type:
     *
     *> val a_pair = poly (fn a => tuple (var a, var a))
     *)
    val poly: 
	(* G,X::* |- T2::K2
	 * -----------------
	 * G |- (\X::*.T2)::*=>K2
	 *)
	(('G,'K1) typevar -> ('G,'K2) typet)
	-> 
	('G,'K1->'K2) typet

(*
    val any:
	(* G,X::K1 |- T2::K2
	 * -----------------
	 * G |- (any\X::K1.T2)::(K1=>K2)
	 *)
	(('G,'K1) typevar -> ('G,'K2) typet)
	->
	('G,'K1->'K2) typet

    val some:
	(* G |- T1::K1    G |- T2::K2
	 * -----------------
	 * G |- (some\X::K1.T2)::(K1=>K2)
	 *)
	(('G,'K1) typevar * ('G,'K1) typet -> ('G,'K2) typet)
	->
	('G,'K1->'K2) typet
*)

    (* Constructs a type by applying an argument.
     * 
     * For example, to create an instance of the
     * polymorphic pair shown above:
     *
     *> val bool_pair = apply (a_pair, bool)
     *)
    val apply: 
	(* G |- T1::(K11=>K12)    G |- T2::K11
	 * --------------------------------
	 * G |- (T1 T2)::K12
	 *)
	('G,'K11->'K12) typet * ('G,'K11) typet
	-> 
	('G,'K12) typet

    (* A function type constructor. *)
    val arrow: 
	(* G |- T1::*    G |- T2::*
	 * ------------------------
	 * G |- (T1->T2)::*
	 *)
	('G,proper) typet * ('G,proper) typet 
	-> 
	('G,proper) typet

    val record:
	(* G |- T1::*    G |- T2::*
         * -----------------------
	 * G |- {T1;T2}::*
	 *)
	(field * ('G,proper) typet) * (field * ('G,proper) typet)
	->
	('G,proper) typet

    val recordv:
	(* G |- T[1..N]::*
	 * ---------------
	 * G |- {T1;T2;...;T3}::*
	 *)
	(field * ('G,proper) typet) vector
	->
	('G,proper) typet

    (*= Utilities =*)

    val rep: ('G,'K) typet -> rep
    (* Recovers the concrete representation of a type. *)

    val gen: ('G,'K) typet -> t
    (* Relaxes the kinding restriction. *)

    val isProper: ('G,'K) typet -> bool
    val isConstructor: ('G,'K) typet -> bool

    val varName: var -> string

    val normal: ('G,'K) typet -> ('G,'K) typet
    val subtype: ('G,'K) typet * ('G,'K) typet -> bool
    val equiv: ('G,'K) typet * ('G,'K) typet -> bool

    val toLayout: ('G,'K) typet -> Layout.t
end



