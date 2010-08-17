(* SimEngine Algorithmic Intermediate Language
 * 
 * SAIL is a lazy, polymorhpic, explicitly-typed, higher-order,
 * functional intermediate form, providing an Algorithmic Skeleton
 * framework which allows the programmer to explicitly specify control
 * structures for task- and data-parallel applications.
 * 
 * References
 * http://en.wikipedia.org/wiki/Algorithmic_skeleton
 * http://www.di.unipi.it/~susanna/p3l.html
 * http://www.mathematik.uni-marburg.de/~eden/
 * http://skandium.niclabs.cl/
 *)
signature SAIL = sig
    (* The dimensionality of a datum *)
    type size
    (* A variable identifier *)
    type ident
    (* A program label *)
    type label
    (* A literal value *)
    type immediate

    structure Type: sig
	datatype t
	  = PRIMITIVE of primitive_t
	  | VECTOR of primitive_t * size
	  (* A return type and a list of parameter types. *)
	  | ARROW of t vector * t
	  (* Applies a list of types to a polymorphic type. *)
	  | CONSTRUCT of t vector * ident
	  | IDENT of ident

	     and primitive_t
	       = VOID
	       | INTEGER of size
	       | FLOAT of size
	       | PREDICATE
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

    structure Lambda: sig
	type t
	type expression
    end

    structure Expression: sig
	type t
    end
    sharing type Expression.t = Lambda.expression

    structure Binding: sig
	type lambda
	type expression
	datatype t
	  = Function of {var: ident * Type.t,
			 object: lambda}
	  | Value of {var: ident * Type.t,
		      object: expression}

    end
    sharing type Binding.lambda = Lambda.t
    sharing type Binding.expression = Lambda.expression

    structure Task: sig
	type lambda
	datatype t
	  = If of {condition: lambda,
		   task: t,
		   otherwise: t option}
	  | For of {count: int, 
		    task: t}
	  | While of {condition: lambda,
		      task: t}
	  | Map of {divide: lambda,
		    task: t,
		    merge: lambda}
	  | Reduce of {divide: lambda,
		       task: t, 
		       merge: lambda}
	  (* Left-associative, exclusive cumulative sum. *)
	  | Scanl of {divide: lambda,
		      task: t,
		      merge: lambda}
	  | DivideAndConquer of {divisible: lambda,
				 divide: lambda,
				 task: t,
				 merge: lambda}
	  | Fork of {divide: lambda, 
		     tasks: t vector, 
		     merge: lambda}
	  (* Injects sequential code into a leaf node of a parallel program. *)
	  | Sequence of lambda
	  | Pipe of t * t
    end
    sharing type Task.lambda = Lambda.t

    structure Operator: sig
	type t
    end

    structure Atom: sig
	type expression
	type lambda
	type binding
	type task
	type operator
	datatype t
	  = Abstraction of lambda
	  | Application of expression * expression
	  | Primitive of operator * expression vector
	  | Let of binding vector * expression
	  | Variable of ident
	  | Literal of immediate
	  | Task of task
    end
    sharing type Atom.expression = Lambda.expression
    sharing type Atom.lambda = Lambda.t
    sharing type Atom.task = Task.t
    sharing type Atom.binding = Binding.t
    sharing type Atom.operator = Operator.t

    structure Program: sig
	datatype t
	  = PROGRAM of {body: Expression.t,
			types: TypeDeclaration.t vector}
    end
end
