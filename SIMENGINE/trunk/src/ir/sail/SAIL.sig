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

    structure Type: sig
	datatype t
	  = PRIMITIVE of primitive_t
	  | VECTOR of primitive_t * size
	  (* A return type and a list of parameter types. *)
	  | FUNCTION of t * (t vector)
	  (* Applies a list of types to a polymorphic type. *)
	  | CONSTRUCT of ident * (t vector)
	  | IDENT of ident

	     and primitive_t
	       = Void
	       | Integer of size
	       | Float of size
	       | Predicate
	       | Label 
	       | Address
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
	
    end

    structure Atom: sig
	datatype t
	  = Null
	  | Variable of ident
	  | Literal of immediate
	  | Abstraction of ident vector * t
	  | Application of t * t
    end


    structure Function: sig
	datatype t
    end

    structure Task: sig
	datatype t
	  = If of {condition: Function.t,
		   task: t,
		   otherwise: t option}
	  | For of {count: int, 
		    task: t}
	  | While of {condition: Function.t,
		      task: t}
	  | Map of {divide: Function.t,
		    task: t,
		    merge: Function.t}
	  | Reduce of {divide: Function.t,
		       task: t, 
		       merge: Function.t}
	  (* Left-associative, exclusive cumulative sum. *)
	  | Scanl of {divide: Function.t,
		      task: t,
		      merge: Function.t}
	  | DivideAndConquer of {divisible: Function.t,
				 divide: Function.t,
				 task: t,
				 merge: Function.t}
	  | Fork of {divide: Function.t, 
		     tasks: t vector, 
		     merge: Function.t}
	  (* Injects sequential code into a leaf node of a parallel program. *)
	  | Sequence of Function.t
	  | Pipe of t * t
    end



end
