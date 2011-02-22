structure Sail: SAIL = struct

type size = int
type ident = string
type label = string
type address = int


(* create a superset of Layout functions *)
structure Layout= struct
  open Layout
  val i2l = str o Int.toString
  val r2l = str o Real.toString
  val b2l = str o Util.b2s
  fun v2l v = 
      List.tabulate (Vector.length v, (fn(i)=> Vector.sub (v, i)))
  fun vectorMap mapfun v =
      bracketList (v2l (Vector.map mapfun v))
  fun comment t = 
      seq [str " (* ", t, str " *) "]
  fun multiline_comment tlist = 
      align ([str "(*"] @
	     (map (fn(t)=> seq [str " * ", t]) tlist) @
	     [str " *)"])
  fun sub t = indent (t, 3)  
  fun smlfun (id, args, body) =
      mayAlign [seq [str "fun ",
		     id,
		     parenList args,
		     str " = "],
		sub body]
  fun smlfn (args, body) =
      mayAlign [seq [str "fn ",
		     parenList args,
		     str " => "],
		sub body]
  fun smllet ([], expression) = 
      expression
    | smllet ([binding], expression) =
      mayAlign [seq [str "let ", binding],
		seq [str "in ", expression],
		str "end"]
    | smllet (bindings, expression) =
      align [str "let",
	     sub (align bindings),
	     str "in",
	     sub expression,
	     str "end"]
  fun smlval (lhs, rhs) = 
      mayAlign [seq [str "val ", lhs, str " = "],
		rhs]
  fun smlif (cond, ift, iff) =
      mayAlign [seq [str "if ", cond, str " then "],
		sub (ift),
		str "else",
		sub (iff)]
      
end


structure Immediate = struct

datatype t 
  = String of string
  | Real of real
  | Int of int
  | Bool of bool

local
    open Layout
in
fun toLayout (String s) = label ("immediate", str s)
  | toLayout (Real r) = label ("immediate", r2l r)
  | toLayout (Int i) = label ("immediate", i2l i)
  | toLayout (Bool b) = label ("immediate", b2l b)

fun toSML (String s) = seq [str "\"", str s, str "\""]
  | toSML (Real r) = r2l r
  | toSML (Int i) = i2l i
  | toSML (Bool b) = b2l b
end

end

datatype immediate = datatype Immediate.t
val immediateToLayout = Immediate.toLayout
val immediateToSML = Immediate.toSML

local
    open Layout
in
fun sizeToLayout s = label ("size", i2l s)
fun identToLayout i = label ("ident", str i)
fun labelToLayout l = label ("label", str l)
fun addressToLayout a = label ("address", i2l a)

fun sizeToSML s = bracket (i2l s)
fun identToSML i = str i
fun labelToSML l = str l
fun addressToSML a = i2l a
end


(* Layout the SAIL data structure *)
local
    open Layout
in
fun typeToLayout t = label ("Type", Type.toLayout t)
fun typeToSML t = Type.toSML t
end


structure Type = struct
type t = (Type.context, Type.kind) Type.typet
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
local
    open Layout
in
fun toLayout (PARAMETRIC {name, vars, base}) =
    heading ("Parametric", align [label ("name", str name),
				  label ("vars", vectorMap str vars),
				  label ("base", Type.toLayout base)])
  | toLayout (ARRAY {name, vars, base}) =
    heading ("Array", align [label ("name", str name),
			     label ("vars", vectorMap str vars),
			     label ("base", Type.toLayout base)])
  | toLayout (STRUCTURE {name, vars, fields}) =
    heading ("Structure", align [label ("name", str name),
				 label ("vars", vectorMap str vars),
				 label ("fields", vectorMap (fn(id, typ)=> paren (seq [str id, str ": ", Type.toLayout typ])) fields)])
fun toSML _ = str "???TypeDeclaration???"
end

end


datatype task
  (* We augment the familiar lambda calculus with a few additional
   * abstraction operators representing parallel and sequential control.
   *)
  = Lambda of {param: (ident * Type.t) vector, body: expression}
  (* Typical lambda abstraction with variable parameter arity. 
   * fun LAMBDA (params, body) = fn(params)=>body *)

  | Pipe of task * task
  (* Continuation abstraction.
   * The result of the first is the argument to the second.
   * The result of the second is the result of the whole.
   * The parameters of the first are the parameters of the whole.
   * fun PIPE (t1, t2) args = (t2 o t1) args
   *)

  | If of {condition: atom, task: task, otherwise: task}
  (* Conditional branching.
   * fn (args) => if p then ift (args) else iff (args)
   *)

  | For of {count: int, task: task}
  (* Parallel iteration.
   * A number of identical tasks are replicated, 
   * each taking an integer parameter representing its index.
   * fun FOR (count, task) = 
   *   if count <= 0 then
   *     ()
   *   else
   *     FOR (count - 1, (task(count); task))


   fun FOR (count,task) =
       let
           fun loop n = if n = 0 then () else (task n; loop n-1)
       in
	   loop count
       end


   *)

  | While of {condition: task, task: task}
  (* Sequential repetition. 
   * A single (nullary) task is repeated until the condition indicates true.
   * fun WHILE (cond, task) = 
   *   if cond() then
   *     WHILE (cond, (task(); task))
   *   else
   *     ()


fun WHILE (cond, task) =
    let
	fun loop x = if cond () then loop (task ()) else x
    in
	loop ()
    end


   *)

  | Fixpoint of task
  (*
   local
       datatype 'a t = In of 'a t -> 'a
       fun out (In a) = a (In a)
   in
   fun Y f = let fun comb x = f (out x) 
	     in
		 comb (In (fn x => fn a => f (out x) a))
	     end
   end
   
   *)

  | DivideAndConquer of {divisible: task, divide: task, task: task, merge: task}
  (*
   * fn (a) =>
   * let
   *     val divisible = ...
   *     val divide = ...
   *     val task = ...
   *     val merge = ...
   *     fun DivideAndConquer (a) = 
   * 	        if divisible(a) then
   *     	    let
   *     		val (first_half, second_half) = divide(a)
   *     	    in
   *     		merge (DivideAndConquer first_half, DivideAndConquer second_half)
   *     	    end
   *    	else
   *    	    task (a)
   * in
   *     DivideAndConquer a
   * end
   *)

  | Map of {divide: task, task: task, merge: task}
  (* fun MAP (divide, task, merge) = (merge o (map task) o divide) *)

(*
val divisible = fn [] => false | [x] => false | _ => true
fun MAP (divide,task,merge) = 
    DivideAndConquer (divisible, divide, task, merge)
*)


  | Reduce of {divide: task, task: task, merge: task}
  (* fun REDUCE (divide, task, merge) = (merge o (map task) o divide) *)

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
       = Exp of {bindings: binding vector, result: typeapp}
		
     and operator
       = MathFunction of MathFunctions.operation
       | SMLFunction of ident
       | Operator_bug

     and typeapp
       = TypeApply of {var: atom,
		       args: Type.t vector}

(* Layout the SAIL data structure *)
local
    open Layout
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
    
and operatorToLayout (MathFunction oper) = label ("MathFunction", str (MathFunctionProperties.op2name oper))
  | operatorToLayout (SMLFunction oper) = label ("SMLFunction", str oper)
  | operatorToLayout (Operator_bug) = str "Operator_bug"

and typeApplicationToLayout (TypeApply {var, args}) =
    label ("TypeApply", atomToLayout var)

fun taskToSML (Lambda {param, body}) = 
    smlfn (map (fn(id,typ)=> seq [identToSML id,
				  comment (seq [str ":",
						typeToSML typ])]) (v2l param),
	   expressionToSML body)
  | taskToSML (Pipe (t1, t2)) = mayAlign [paren(taskToSML t1),
					  str " o ",
					  paren(taskToSML t2)]
  | taskToSML (If {condition, task, otherwise}) = 
    smlfn ([str "args"],
	   smlif (atomToSML condition,
		  seq [(taskToSML task), str " (args)"],
		  seq [(taskToSML otherwise), str " (args)"]))
  | taskToSML (For {count, task}) = str "For"
  | taskToSML (While {condition, task}) = str "While"
  | taskToSML (Fixpoint t) = str "Fixpoint"
  | taskToSML (DivideAndConquer {divisible, divide, task, merge}) = 
    smlfn ([str "a"],
	   smllet ([smlval (str "divisible", taskToSML divisible),
		    smlval (str "divide", taskToSML divide),
		    smlval (str "task", taskToSML task),
		    smlval (str "merge", taskToSML merge),
		    smlfun (str "DivideAndConquer", [str "a"],
			    smlif (str "divisible(a)",
				   smllet ([smlval (parenList [str "first_half", 
							       str "second_half"], 
						    str "divide(a)")],
					   str "merge (DivideAndConquer first_half, DivideAndConquer second_half)"),
				   str "task(a)"))],
		   str "DivideAndConquer a"))
  | taskToSML (Map {divide, task, merge}) = str "Map"
  | taskToSML (Reduce {divide, task, merge}) = str "Reduce"
  | taskToSML (Scanl {divide, task, merge}) = str "Scanl"
  | taskToSML (Fork {divide, tasks, merge}) = str "Fork"
and expressionToSML (Exp {bindings, result}) = 
    let val binding_list = v2l bindings
    in smllet (map bindingToSML binding_list,
	       typeApplicationToSML result)
    end

and atomToSML (Variable id) = identToSML id
  | atomToSML (Abstract task) = taskToSML task
  | atomToSML (Apply (ta1, ta2)) = seq [paren (typeApplicationToSML ta1),
					paren (typeApplicationToSML ta2)]
  | atomToSML (Primitive (oper, ta_vec)) = 
    seq [operatorToSML oper,
	 parenList (v2l (Vector.map typeApplicationToSML ta_vec))]
  | atomToSML (Literal imm) = immediateToSML imm

and bindingToSML (Value {var=(id,typ), object}) = seq [str "val ", 
						       identToSML id,
						       comment(seq[str " : ",
								   typeToSML typ]),
						       str " = ",
						       atomToSML object]
  | bindingToSML (PolyValue {tyvars, var, object}) = comment (str "PolyValue binding")
  | bindingToSML (Function {tyvars, var=(id,typ), object}) = 
    smlfun (seq [identToSML id, 
		 comment (seq [str ":", typeToSML typ])],
	    v2l (Vector.map identToSML tyvars),
	    taskToSML object)

and operatorToSML (MathFunction oper) = str (MathFunctionProperties.op2name oper)
  | operatorToSML (SMLFunction oper) = str oper
  | operatorToSML (Operator_bug) = str "Operator_bug"

and typeApplicationToSML (TypeApply {var, args}) =
    let
	val usetypes = false
    in
	if usetypes then
	    seq [atomToSML var,
		 if Vector.length args = 0 then
		     empty
		 else
		     seq [str " : ",
			  if Vector.length args = 1 then
			      typeToSML (Vector.sub (args, 0))
			  else
			      str "???UNEXPECTED_VECTOR_TYPE???"]]
	else
	    atomToSML var
    end

end

structure TypeApplication = struct

datatype atom = datatype atom

datatype t = datatype typeapp
val toLayout = typeApplicationToLayout
val toSML = typeApplicationToSML
end

structure ArrayOperators = struct
val null = fn x => Primitive (SMLFunction "array_null", Vector.fromList [x])
val split = fn x => Primitive (SMLFunction "array_split", Vector.fromList [x])
val sort = fn x => Primitive (SMLFunction "array_sort", Vector.fromList [x])
val concat = fn (x,y) => Primitive (SMLFunction "array_concat", Vector.fromList [x,y])
end

structure PairOperators = struct
val one = fn x => Primitive (SMLFunction "pair_one", Vector.fromList [x])
val two = fn x => Primitive (SMLFunction "pair_two", Vector.fromList [x])
end

structure Operator = struct
datatype t = datatype operator
datatype typeapp = datatype TypeApplication.t
datatype atom = datatype atom
structure Array = ArrayOperators
structure Pair = PairOperators
val toLayout = operatorToLayout
val toSML = operatorToSML
end

structure Abstraction = struct
datatype atom = datatype atom
datatype expression = datatype expression
datatype t = datatype task		      

val toLayout = taskToLayout
val toSML = taskToSML
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

local
    open Layout
in
fun toLayout (PROGRAM {body, types}) = 
    heading ("Program", 
	     align [heading ("body", expressionToLayout body),
		    heading ("types", vectorMap TypeDeclaration.toLayout types)])
end

end




end



		       

