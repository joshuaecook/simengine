structure Sail: SAIL = struct

type size = int
type ident = string
type label = string
type immediate = unit
type address = int

local
open Layout
val i2l = str o Int.toString
in
fun sizeToLayout s = label ("size", i2l s)
fun identToLayout i = label ("ident", str i)
fun labelToLayout l = label ("label", str l)
fun immediateToLayout i = label ("immediate", str "unit")
fun addressToLayout a = label ("address", i2l a)
end


(* Layout the SAIL data structure *)
local
    open Layout
in
fun typeToLayout t = label ("Type", str "Undefined type")
end

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
val toLayout = typeToLayout
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
local
open Layout
in
fun toLayout (TypeApply {var, args}) = 
    label ("TypeApply", str var)
end
end

val typeApplicationToLayout = TypeApplication.toLayout



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


(* Layout the SAIL data structure *)
local
    open Layout
    fun v2l v = 
	List.tabulate (Vector.length v, (fn(i)=> Vector.sub (v, i)))
in
fun taskToLayout (Lambda {param, body}) = 
    heading ("Lambda",
	     align [heading ("param", 
			     bracketList (map (fn(id, typ)=>
						 align [identToLayout id,
							Type.toLayout typ]) (v2l param))),
		    heading ("body", expressionToLayout body)])
  | taskToLayout (Pipe (t1, t2)) = str "Pipe"
  | taskToLayout (If {condition, task, otherwise}) = str "If"
  | taskToLayout (For {count, task}) = str "For"
  | taskToLayout (While {condition, task}) = str "While"
  | taskToLayout (Fixpoint t) = str "Fixpoint"
  | taskToLayout (DivideAndConquer {divisible, divide, task, merge}) = 
    heading ("DivideAndConquer",
	     align [heading ("divisible", taskToLayout divisible),
		    heading ("divide", taskToLayout divide),
		    heading ("task", taskToLayout task),
		    heading ("merge", taskToLayout merge)])
  | taskToLayout (Map {divide, task, merge}) = str "Map"
  | taskToLayout (Reduce {divide, task, merge}) = str "Reduce"
  | taskToLayout (Scanl {divide, task, merge}) = str "Scanl"
  | taskToLayout (Fork {divide, tasks, merge}) = str "Fork"
and expressionToLayout (Exp {bindings, result}) = 
    heading ("Expression", 
	     align [heading ("bindings",
			     bracketList (map bindingToLayout (v2l bindings))),
		    heading ("result",
			     typeApplicationToLayout result)])
and bindingToLayout (Value {var=(id,typ), object}) = 
    heading ("Value", align [label ("var", parenList [identToLayout id, typeToLayout typ]),
			     heading ("object", atomToLayout object)])

  | bindingToLayout (PolyValue {tyvars, var=(id,typ), object}) = 
    heading ("PolyValue", align [label ("tyvars", bracketList (map identToLayout (v2l tyvars))),
				 label ("var", parenList [identToLayout id, typeToLayout typ]),
				 heading ("object", expressionToLayout object)])
  | bindingToLayout (Function {tyvars, var=(id,typ), object}) = 
    heading ("Function", align [label ("tyvars", bracketList (map identToLayout (v2l tyvars))),
				label ("var", parenList [identToLayout id, typeToLayout typ]),
				heading ("object", taskToLayout object)])

and atomToLayout (Variable id) = heading ("Variable", identToLayout id)
  | atomToLayout (Abstract task) = heading ("Abstract", taskToLayout task)
  | atomToLayout (Apply (ta1, ta2)) = heading ("Apply", parenList [typeApplicationToLayout ta1,
								   typeApplicationToLayout ta2])
  | atomToLayout (Primitive (oper, typapps)) = 
    heading ("Primitive", parenList [label ("operator", operatorToLayout oper),
				     label ("typeapps", bracketList (map typeApplicationToLayout (v2l typapps)))])
  | atomToLayout (Literal imm) = 
    heading ("Literal", immediateToLayout imm)
    
and operatorToLayout (Operator_bug) = str "Operator_bug"
end


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
val toLayout = operatorToLayout
end

structure Abstraction = struct
datatype atom = datatype atom
datatype expression = datatype expression
datatype t = datatype task		      

val toLayout = taskToLayout
end


structure Atom = struct
datatype t = datatype atom
datatype typeapp = datatype typeapp
datatype task = datatype task
datatype operator = datatype operator
val toLayout = atomToLayout
end

structure Binding = struct
datatype t = datatype binding
    datatype task = datatype task
    datatype atom = datatype atom
    datatype expression = datatype expression
    val toLayout = bindingToLayout
end

structure Expression = struct
    datatype binding = datatype binding
    datatype t = datatype expression
    val new = Exp
    (*val new: {bindings: binding vector, result: TypeApplication.t} -> t*)

    val toLayout = expressionToLayout

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



		       

