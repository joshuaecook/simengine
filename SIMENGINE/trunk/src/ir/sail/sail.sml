structure Sail: SAIL = struct

type size = int
type ident = string
type label = string
type immediate = unit
type address = int

structure Type = struct
datatype t
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


datatype expression
  = Expression of {bindings: binding vector, result: TypeApplication.t}

     and atom
       = Variable of ident
       | Abstract of lambda
       | Apply of typeapp * typeapp
       | Primitive of operator * typeapp vector
       | Literal of immediate

     and lambda
       = Lambda of {param: (ident * Type.t) vector,
		    body: expression}
       | If of {condition: lambda,
		task: lambda,
		otherwise: lambda option}
       | For of {count: int, 
		 task: lambda}
       | While of {condition: lambda,
		   task: lambda}
       | Map of {divide: lambda,
		 task: lambda,
		 merge: lambda}
       | Reduce of {divide: lambda,
		    task: lambda, 
		    merge: lambda}
       | Scanl of {divide: lambda,
		   task: lambda,
		   merge: lambda}
       | DivideAndConquer of {divisible: lambda,
			      divide: lambda,
			      task: lambda,
			      merge: lambda}
       | Fork of {divide: lambda, 
		  tasks: lambda vector, 
		  merge: lambda}
       | Pipe of lambda * lambda


     and binding
       = Value of {var: ident * Type.t,
		   object: atom}
       | PolyValue of {tyvars: ident vector,
		       var: ident * Type.t,
		       object: expression}
       | Function of {tyvars: ident vector,
		      var: ident * Type.t,
		      object: lambda}

     and operator
       = Operator_bug


structure Abstraction = struct
datatype t = datatype lambda
datatype expression = datatype expression
end

structure Expression = struct
datatype t = datatype expression
datatype binding = datatype binding
val new = Expression
end

structure Binding = struct
datatype t = datatype binding
datatype lambda = datatype lambda
datatype atom = datatype atom
datatype expression = datatype expression
end

structure ListOperators = struct
val null = fn x => Primitive (Operator_bug, #[x])
val split = fn x => Primitive (Operator_bug, #[x])
val sort = fn x => Primitive (Operator_bug, #[x])
val concat = fn (x,y) => Primitive (Operator_bug, #[x,y])
end

structure PairOperators = struct
val one = fn x => Primitive (Operator_bug, #[x])
val two = fn x => Primitive (Operator_bug, #[x])
end

structure Operator = struct
datatype t = datatype operator
datatype typeapp = datatype TypeApplication.t
datatype atom = datatype atom
structure List = ListOperators
structure Pair = PairOperators
end

structure Atom = struct
datatype t = datatype atom
datatype typeapp = datatype TypeApplication.t
datatype lambda = datatype lambda
datatype binding = datatype binding
datatype operator = datatype operator
end

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
			    structure L = Lambda
			    structure Exp = Expression
			    structure B = Binding

			    val a_t = Type.IDENT "a"
			    val a_var = fn x => TypeApplication.TypeApply {var= x, args= #[a_t]}

			    val aa_t = Type.PAIR (a_t, a_t)
			    val aa_var = fn x => TypeApplication.TypeApply {var= x, args= #[aa_t]}

			    val simple_list_fun 
			      = fn oper =>
				   let val body
					 = Exp.new {bindings= #[B.Value {var= ("y", a_t), 
									 object= oper (a_var "x")}], 
						    result= a_var "y"}
				   in
				       L.Lambda {param= ("x", a_t),
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
					  = Exp.new {bindings= #[B.Value {var= ("x", a_t), object= Op.Pair.one (aa_var "xy")},
								 B.Value {var= ("y", a_t), object= Op.Pair.two (aa_var "xy")},
								 B.Value {var= ("z", a_t), object= Op.List.concat (a_var "x", a_var "y")}],
						     result= a_var "z"}
				    in
					L.Lambda {param= ("xy", a_t),
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
