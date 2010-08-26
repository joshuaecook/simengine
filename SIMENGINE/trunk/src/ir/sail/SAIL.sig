(* SimEngine Algorithmic Intermediate Language
 * 
 * SAIL is a lazy, polymorhpic, explicitly-typed, higher-order,
 * functional intermediate form, providing an Algorithmic Skeleton
 * framework which allows the programmer to explicitly specify control
 * structures for task- and data-parallel applications.
 * 
 * Typing is explicit: each expression is paired with its type scheme.
 * 
 * References
 * http://en.wikipedia.org/wiki/Algorithmic_skeleton
 * http://www.di.unipi.it/~susanna/p3l.html
 * http://www.mathematik.uni-marburg.de/~eden/
 * http://skandium.niclabs.cl/
 *)

(*
signature IDENTITY = sig
    (* A type of object having a unique identity. *)
    type t
end

signature TYPE_CONSTRUCTOR = sig
    include IDENTITY
    type size
    type tycon = t

    val array: tycon
    val arrow: tycon
    val bool: tycon
    val int: size -> tycon
    val float: size -> tycon
    val reference: tycon
    val tuple: tycon
    val word: size -> tycon
    val vector: tycon
end

signature TYPE_VARIABLE = sig
    type t

    val name: t -> string
end

signature TYPE_OPERATOR = sig
    type t

    val array: t -> t
    val arrow: t * t -> t
    val bool: t
    val int32: t
    val float32: t
    val float64: t
    val reference: t -> t
    val tuple: t vector -> t
    val word32: t
    val vector: t -> t

end

signature TYPE_ENVIRONMENT = sig
    type ident
    structure Tyvar: TYPE_VARIABLE
    structure Tyop: TYPE_OPERATOR
end
*)
(*
signature TYPE = sig
    type ident
    type tycon
    type t = unit

    val var: ident -> t
    (* Creates a new variable type *)

    val tycon: t -> tycon option
end
*)
signature TYPE = sig
    type t = unit
end

signature SAIL = sig
    type size
    (* The dimensionality of a datum *)
    type ident
    (* A variable identifier *)
    type label
    (* A program label *)
    type immediate
    (* A literal value *)

    structure Type: sig
	type t
    end

    structure TypeDeclaration: sig
	datatype t 
	  = PARAMETRIC of {name: ident,
			   vars: ident vector,
			   base: Type.t}
	  | ARRAY of {name: ident,
		      vars: ident vector,
		      base: Type.t}
	  | STRUCTURE of {name: ident,
			  vars: ident vector,
			  fields: (ident * Type.t) vector}
    end

    structure TypeApplication: sig
	datatype t
	  = TypeApply of {var: ident,
			  args: Type.t vector}
    end

    structure Operator: sig
	type typeapp

	type t
	type atom

	structure Array: sig
	    val null: typeapp -> atom
	    val split: typeapp -> atom
	    val sort: typeapp -> atom
	    val concat: typeapp * typeapp -> atom
	end
	structure Pair: sig
	    val one: typeapp -> atom
	    val two: typeapp -> atom
	end
   end

    structure Abstraction: sig
	type atom
	type expression

	datatype t
	  (* We augment the familiar lambda calculus with a few additional
	   * abstraction operators representing parallel and sequential control.
	   *)
	  = Lambda of {param: (ident * Type.t) vector, body: expression}
	  (* Typical lambda abstraction with variable parameter arity. *)

	  | Pipe of t * t
	  (* Continuation abstraction.
	   * The result of the first is the argument to the second.
	   * The result of the second is the result of the whole.
	   * The parameters of the first are the parameters of the whole.
	   *)

	  | If of {condition: t, task: t, otherwise: t option}
	  (* Conditional branching.
	   * The result is NULL if condition indicates false and otherwise is NONE
	   *)

	  | For of {count: int, task: t}
	  (* Parallel iteration.
	   * A number of identical tasks are replicated, 
	   * each taking an integer parameter representing its index.
	   *)

	  | While of {condition: t, task: t}
	  (* Sequential repetition. 
	   * A single (nullary) task is repeated until the condition indicates true.
	   *)

	  | Fixpoint of t

	  | DivideAndConquer of {divisible: t, divide: t, task: t, merge: t}

	  | Map of {divide: t, task: t, merge: t}

	  | Reduce of {divide: t, task: t, merge: t}

	  | Scanl of {divide: t, task: t, merge: t}
	  (* Left-associative, exclusive cumulative sum. *)

	  | Fork of {divide: t, tasks: t vector, merge: t}
          (* Control parallelism. *)
    end

    structure Atom: sig
	type typeapp
	type task
	type operator

	datatype t
	  = Variable of ident
	  | Abstract of task
	  | Apply of typeapp * typeapp
	  | Primitive of operator * typeapp vector
	  | Literal of immediate
    end

    structure Binding: sig
	type task
	type atom
	type expression
	datatype t
	  = Value of {var: ident * Type.t,
		      object: atom}
	  | PolyValue of {tyvars: ident vector,
			  var: ident * Type.t,
			  object: expression}
	  | Function of {tyvars: ident vector,
			 var: ident * Type.t,
			 object: task}
    end

    structure Expression: sig
	type binding
	type t
	val new: {bindings: binding vector, result: TypeApplication.t} -> t
    end

    sharing type Operator.atom = Atom.t
    sharing type Atom.typeapp = TypeApplication.t
    sharing type Atom.task = Abstraction.t
    sharing type Atom.operator = Operator.t
    sharing type Binding.task = Abstraction.t
    sharing type Binding.expression = Abstraction.expression
    sharing type Expression.t = Abstraction.expression
    sharing type Expression.binding = Binding.t

    structure Program: sig
	datatype t
	  = PROGRAM of {body: Expression.t,
			types: TypeDeclaration.t vector}
    end
end
