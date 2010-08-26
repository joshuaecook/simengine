structure Sail: SAIL = struct

type size = int
type ident = string
type label = string
type immediate = unit
type address = int

structure Type = struct
type t = unit
(*
  = VOID
  | PRIMITIVE of primitive_t
  | VECTOR of primitive_t * size
  | PAIR of t * t
  | ARROW of t vector * t
  | VAR of ident

     and primitive_t
       = INTEGER of size
       | FLOAT of size
       | WORD of size
       | PREDICATE
*)
end

structure TypeDeclaration = struct
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

structure TypeApplication = struct
datatype t
  = TypeApply of {var: ident,
		  args: Type.t vector}
end


datatype typeapp = datatype TypeApplication.t

    datatype task
      (* We augment the familiar lambda calculus with a few additional
       * abstraction operators representing parallel and sequential control.
       *)
      = Lambda of {param: (ident * Type.t) vector, body: expression}
      (* Typical lambda abstraction with variable parameter arity. *)

      | Pipe of task * task
      (* Continuation abstraction.
       * The result of the first is the argument to the second.
       * The result of the second is the result of the whole.
       * The parameters of the first are the parameters of the whole.
       *)

      | If of {condition: task, task: task, otherwise: task option}
      (* Conditional branching.
       * The result is NULL if condition indicates false and otherwise is NONE
       *)

      | For of {count: int, task: task}
      (* Parallel iteration.
       * A number of identical tasks are replicated, 
       * each taking an integer parameter representing its index.
       *)

      | While of {condition: task, task: task}
      (* Sequential repetition. 
       * A single (nullary) task is repeated until the condition indicates true.
       *)

      | Fixpoint of task

      | DivideAndConquer of {divisible: task, divide: task, task: task, merge: task}

      | Map of {divide: task, task: task, merge: task}

      | Reduce of {divide: task, task: task, merge: task}

      (* Left-associative, exclusive cumulative sum. *)
      | Scanl of {divide: task, task: task, merge: task}

      (* Control parallelism. *)
      | Fork of {divide: task, tasks: task vector, merge: task}

    and atom
      = Variable of ident
      | Abstract of task
      | Apply of typeapp * typeapp
      | Primitive of operator * typeapp vector
      | Literal of immediate

    and binding
      = Value of {var: ident * Type.t,
		  object: atom}
      | PolyValue of {tyvars: ident vector,
		      var: ident * Type.t,
		      object: expression}
      | Function of {tyvars: ident vector,
		     var: ident * Type.t,
		     object: task}
    and expression
      = Exp of {bindings: binding vector, result: TypeApplication.t}
	     
    and operator
      = Operator_bug

structure ArrayOperators = struct
val null = fn x => Primitive (Operator_bug, Vector.fromList [x])
val split = fn x => Primitive (Operator_bug, Vector.fromList [x])
val sort = fn x => Primitive (Operator_bug, Vector.fromList [x])
val concat = fn (x,y) => Primitive (Operator_bug, Vector.fromList [x,y])
end

structure PairOperators = struct
val one = fn x => Primitive (Operator_bug, Vector.fromList [x])
val two = fn x => Primitive (Operator_bug, Vector.fromList [x])
end

structure Operator = struct
datatype t = datatype operator
datatype typeapp = datatype TypeApplication.t
datatype atom = datatype atom
structure Array = ArrayOperators
structure Pair = PairOperators
end

structure Abstraction = struct
datatype atom = datatype atom
datatype expression = datatype expression
datatype t = datatype task
end


structure Atom = struct
datatype t = datatype atom
datatype typeapp = datatype typeapp
datatype task = datatype task
datatype operator = datatype operator
end

structure Binding = struct
datatype t = datatype binding
    datatype task = datatype task
    datatype atom = datatype atom
    datatype expression = datatype expression
end

structure Expression = struct
    datatype binding = datatype binding
    datatype t = datatype expression
    val new = Exp
    (*val new: {bindings: binding vector, result: TypeApplication.t} -> t*)
end


(*
sharing type Operator.atom = Atom.t
sharing type Atom.typeapp = TypeApplication.t
sharing type Atom.task = Abstraction.t
sharing type Atom.operator = Operator.t
sharing type Binding.task = Abstraction.t
sharing type Binding.expression = Abstraction.expression
sharing type Expression.t = Abstraction.expression
sharing type Expression.binding = Binding.t
sharing type Abstraction.atom = Atom.t
sharing type Abstraction.task = Task
*)
structure Program = struct
datatype t
  = PROGRAM of {body: Expression.t,
		types: TypeDeclaration.t vector}
end

end

(*
		       
local
    open Sail
    structure Op = Operator
    structure A = Atom
    (*structure L = lambda*)
    structure Exp = Expression
    structure B = Binding

    val a_t = () (*Type.VAR "a"*)
    val a_var = fn x => TypeApplication.TypeApply {var= x, args= Vector.fromList [a_t]}

    val aa_t = () (*Type.PAIR (a_t, a_t)*)
    val aa_var = fn x => TypeApplication.TypeApply {var= x, args= Vector.fromList [aa_t]}

    val simple_list_fun 
      = fn oper =>
	   let val body
		 = Exp.new {bindings= Vector.fromList [B.Value {var= ("y", a_t), 
								object= oper (a_var "x")}], 
			    result= a_var "y"}
	   in
	       Lambda {param= ("x", a_t),
		       body= body}
	   end

in
val mergesort =
    let
	val divisible = simple_list_fun Op.List.null
	val divide = simple_list_fun Op.List.split
	val sort = simple_list_fun Op.List.sort

	val merge
	  = let val body
		  = Exp.new {bindings= Vector.fromList [B.Value {var= ("x", a_t), object= Op.Pair.one (aa_var "xy")},
							B.Value {var= ("y", a_t), object= Op.Pair.two (aa_var "xy")},
							B.Value {var= ("z", a_t), object= Op.List.concat (a_var "x", a_var "y")}],
			     result= a_var "z"}
	    in
		Lambda {param= ("xy", a_t),
			body= body}
	    end
    in
	Task.DivideAndConquer {divisible=divisible, 
			       divide=divide,
			       task=Task.Sequence sort,
			       merge=merge}
    end
end
		       
*)
