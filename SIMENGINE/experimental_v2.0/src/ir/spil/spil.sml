structure Spil: SPIL = struct

type size = int
type ident = string
type label = string

datatype immediate 
  = Real of string
  | Int of string
  | Bool of string
  | String of string
  | Const of ident
  | Nan
  | Infinity
  | Undefined

type address = string

structure TypeDeclaration = struct
type t = unit
end



datatype atom
  = Null
  | Void
  | Literal of immediate
  | Variable of ident
  | Source of atom
  | Sink of atom
  | RuntimeVar of ((unit -> atom) * Type.t)
  | CompileVar of ((unit -> atom) * Type.t)
  | Address of address
  | Symbol of ident
  | Label of ident
  | Cast of atom * Type.t

     and operator
       = Int_add
       | Int_sub
       | Int_mul
       | Int_lt
       | Float_add
       | Float_sub
       | Float_mul
       | Float_neg
       | Float_div
       | Float_gt
       | Float_ge
       | Float_lt
       | Float_le
       | Float_eq
       | Float_ne
       | Math_exp
       | Math_pow
       | Math_sin
       | Math_cos
       | Math_tan
       | Math_csc
       | Math_sec
       | Math_cot
       | Math_asin
       | Math_acos
       | Math_atan
       | Math_atan2
       | Math_acsc
       | Math_asec
       | Math_acot
       | Math_sinh
       | Math_cosh
       | Math_tanh
       | Math_csch
       | Math_sech
       | Math_coth
       | Math_asinh
       | Math_acosh
       | Math_atanh
       | Math_acsch
       | Math_asech
       | Math_acoth
       | Rational_rational
       | Complex_complex
       | Range_range
       | Array_array
       | Array_extract
       | Vector_extract
       | Matrix_dense
       | Matrix_banded
       | Record_record
       | Record_extract
       | Random_uniform
       | Random_normal
       | Address_addr
       | Address_deref
       | Sim_if
       | Sim_input
       | Sim_output
       | Sim_bug

     and expression
       = Value of atom
       | Apply of {oper: operator,
		   args: expression vector}

     and statement
       = Halt
       | Nop
       | Comment of string
       | Profile of ident
       | Bind of {src: atom,
		  dest: ident * Type.t}
       | Graph of {src: expression,
		   dest: ident * Type.t}
       | Primitive of {oper: operator,
		       args: atom vector,
		       dest: ident * Type.t}
       | Move of {src: atom,
		  dest: atom}

     and control
       = Call of {func: ident,
		  args: atom vector, 
		  return: control option} (* NONE for tail calls *)
       | Jump of {block: label,
		  args: atom vector}
       | Switch of {test: atom,
		    cases: (immediate * control) vector,
		    default: control}
       | Return of atom

     and block
       = Block of {label: label,
		   params: (ident * Type.t) vector,
		   body: statement vector,
		   transfer: control}

     and function
       = Function of {params: (ident * Type.t) vector,
		      name: ident,
		      start: label,
		      blocks: block vector,
		      returns: Type.t}

     and program
       = Program of {functions: function vector,
		     main: function,
		     globals: (ident * Type.t) vector,
		     types: TypeDeclaration.t vector}

     and fragment
       = ATOM of atom
       | OPERATOR of operator
       | EXPRESSION of expression
       | STATEMENT of statement
       | CONTROL of control
       | BLOCK of block
       | FUNCTION of function
       | PROGRAM of program
       | TYPENAME of Type.t
       | SERIES of fragment vector (* Comma-delimited *)
       | SEQUENCE of fragment vector (* Semicolon-delimited *)
		   

fun vec f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i)))
val v = Vector.fromList

structure Uses = BinarySetFn(type ord_key = ident val compare = String.compare)
structure Defs = BinarySetFn(type ord_key = ident * Type.t val compare = fn ((a,_),(b,_)) => String.compare (a,b))

structure Context = struct
type atom = atom
type expression = expression
type operator = operator
datatype binding
  = Assumption of Type.t
  | Bind of atom * Type.t
  | Graph of expression * Type.t
  | Primitive of operator * Type.t
type context = (ident * binding) list
val empty = nil
val add = fn (cxt,id,def) => (id,def)::cxt
val rec find =
 fn (nil, name) => NONE
  | ((id,def)::rest, name) => if name = id then SOME def else find (rest,name)
end

structure Environment = struct
type context = Context.context
type expression = expression
datatype binding
  = Assumption of Type.t
  | Function of Type.t
  | Global of expression * Type.t
  | Type of Type.t
type env = (context * ident * binding) list
val empty = nil
val add = fn (env,cxt,id,def) => (cxt,id,def)::env
end

structure Atom = struct
datatype t = datatype atom

val rec uses =
 fn Variable id => SOME id
  | Cast (atom, t) => uses atom
  | Source base => uses base
  | Sink base => uses base
  | RuntimeVar (f, t) => uses (f ())
  | CompileVar (f, t) => uses (f ())
  | _ => NONE
end

structure Operator = struct
datatype expression = datatype expression
datatype t = datatype operator

structure Record = struct
fun extract (expr, field) = Apply {oper= Record_extract, args= v[expr, Value (Symbol field)]}
end
structure Array = struct
fun extract (expr, index) = Apply {oper= Array_extract, args= v[expr, index]}
end
structure Address = struct
fun addr expr = Apply {oper= Address_addr, args= v[expr]}
fun deref expr = Apply {oper= Address_deref, args= v[expr]}
end

val name =
 fn Int_add => "Int_add"
  | Int_sub => "Int_sub"
  | Int_mul => "Int_mul"
  | Int_lt => "Int_lt"
  | Float_add => "Float_add"
  | Float_sub => "Float_sub"
  | Float_mul => "Float_mul"
  | Float_neg => "Float_neg"
  | Float_div => "Float_div"
  | Float_gt => "Float_gt"
  | Float_ge => "Float_ge"
  | Float_lt => "Float_lt"
  | Float_le => "Float_le"
  | Float_eq => "Float_eq"
  | Float_ne => "Float_ne"
  | Math_exp => "Math_exp"
  | Math_pow => "Math_pow"
  | Math_sin => "Math_sin"
  | Math_cos => "Math_cos"
  | Math_tan => "Math_tan"
  | Math_csc => "Math_csc"
  | Math_sec => "Math_sec"
  | Math_cot => "Math_cot"
  | Math_asin => "Math_asin"
  | Math_acos => "Math_acos"
  | Math_atan => "Math_atan"
  | Math_atan2 => "Math_atan2"
  | Math_acsc => "Math_acsc"
  | Math_asec => "Math_asec"
  | Math_acot => "Math_acot"
  | Math_sinh => "Math_sinh"
  | Math_cosh => "Math_cosh"
  | Math_tanh => "Math_tanh"
  | Math_csch => "Math_csch"
  | Math_sech => "Math_sech"
  | Math_coth => "Math_coth"
  | Math_asinh => "Math_asinh"
  | Math_acosh => "Math_acosh"
  | Math_atanh => "Math_atanh"
  | Math_acsch => "Math_acsch"
  | Math_asech => "Math_asech"
  | Math_acoth => "Math_acoth"
  | Rational_rational => "Rational_rational"
  | Complex_complex => "Complex_complex"
  | Range_range => "Range_range"
  | Array_array => "Array_array"
  | Array_extract => "Array_extract"
  | Vector_extract => "Vector_extract"
  | Matrix_dense => "Matrix_dense"
  | Matrix_banded => "Matrix_banded"
  | Record_record => "Record_record"
  | Record_extract => "Record_extract"
  | Random_uniform => "Random_uniform"
  | Random_normal => "Random_normal"
  | Address_addr => "Address_addr"
  | Address_deref => "Address_deref"
  | Sim_if => "Sim_if"
  | Sim_input => "Sim_input"
  | Sim_output => "Sim_output"
  | Sim_bug => "Sim_bug"

val find =
 fn "Int_add" => SOME Int_add
  | "Int_sub" => SOME Int_sub
  | "Int_mul" => SOME Int_mul
  | "Int_lt" => SOME Int_lt
  | "Float_add" => SOME Float_add
  | "Float_sub" => SOME Float_sub
  | "Float_mul" => SOME Float_mul
  | "Float_neg" => SOME Float_neg
  | "Float_div" => SOME Float_div
  | "Float_gt" => SOME Float_gt
  | "Float_ge" => SOME Float_ge
  | "Float_lt" => SOME Float_lt
  | "Float_le" => SOME Float_le
  | "Float_eq" => SOME Float_eq
  | "Float_ne" => SOME Float_ne
  | "Math_exp" => SOME Math_exp
  | "Math_pow" => SOME Math_pow
  | "Math_sin" => SOME Math_sin
  | "Math_cos" => SOME Math_cos
  | "Math_tan" => SOME Math_tan
  | "Math_csc" => SOME Math_csc
  | "Math_sec" => SOME Math_sec
  | "Math_cot" => SOME Math_cot
  | "Math_asin" => SOME Math_asin
  | "Math_acos" => SOME Math_acos
  | "Math_atan" => SOME Math_atan
  | "Math_atan2" => SOME Math_atan2
  | "Math_acsc" => SOME Math_acsc
  | "Math_asec" => SOME Math_asec
  | "Math_acot" => SOME Math_acot
  | "Math_sinh" => SOME Math_sinh
  | "Math_cosh" => SOME Math_cosh
  | "Math_tanh" => SOME Math_tanh
  | "Math_csch" => SOME Math_csch
  | "Math_sech" => SOME Math_sech
  | "Math_coth" => SOME Math_coth
  | "Math_asinh" => SOME Math_asinh
  | "Math_acosh" => SOME Math_acosh
  | "Math_atanh" => SOME Math_atanh
  | "Math_acsch" => SOME Math_acsch
  | "Math_asech" => SOME Math_asech
  | "Math_acoth" => SOME Math_acoth
  | "Rational_rational" => SOME Rational_rational
  | "Complex_complex" => SOME Complex_complex
  | "Range_range" => SOME Range_range
  | "Array_array" => SOME Array_array
  | "Array_extract" => SOME Array_extract
  | "Vector_extract" => SOME Vector_extract
  | "Matrix_dense" => SOME Matrix_dense
  | "Matrix_banded" => SOME Matrix_banded
  | "Record_record" => SOME Record_record
  | "Record_extract" => SOME Record_extract
  | "Random_uniform" => SOME Random_uniform
  | "Random_normal" => SOME Random_normal
  | "Address_addr" => SOME Address_addr
  | "Address_deref" => SOME Address_deref
  | "Sim_if" => SOME Sim_if
  | "Sim_input" => SOME Sim_input
  | "Sim_output" => SOME Sim_output
  | "Sim_bug" => SOME Sim_bug
  | _ => NONE

end

structure Expression = struct
datatype t = datatype expression
datatype atom = datatype atom
datatype operator = datatype operator

fun var ident = Value (Variable ident)

local open Uses in
val rec uses =
 fn Apply {oper, args} => 
    List.foldl union empty (vec uses args)
  | Value atom => 
    (case Atom.uses atom of SOME id => singleton id | NONE => empty)
end
end

structure Statement = struct
datatype t = datatype statement
datatype atom = datatype atom
datatype operator = datatype operator
datatype expression = datatype expression

val defs =
 fn Halt => NONE
  | Nop => NONE
  | Comment _ => NONE
  | Profile _ => NONE
  | Bind {src, dest} => SOME dest
  | Graph {src, dest} => SOME dest
  | Primitive {oper, args, dest} => SOME dest
  | Move {src, dest} => NONE

local open Uses in
val uses =
 fn Halt => empty
  | Nop => empty
  | Comment _ => empty
  | Profile _ => empty
  | Bind {src, dest} =>
    (case Atom.uses src of SOME id => singleton id | NONE => empty)
  | Graph {src, dest} => Expression.uses src
  | Primitive {oper, args, dest} => 
    List.foldl
	(fn (SOME id, set) => add (set, id) | (NONE, set) => set)
	empty (vec Atom.uses args)
  | Move {src, dest} => 
    addList (empty, List.mapPartial (fn x => x) [Atom.uses src, Atom.uses dest])
end
end

structure Control = struct
datatype t = datatype control
datatype atom = datatype atom

local open Uses in
val rec uses =
 fn Call {args, return, ...} =>
    List.foldl
	(fn (SOME id, set) => add (set, id) | (NONE, set) => set)
	(if Option.isSome return then uses (Option.valOf return) else empty)
	(vec Atom.uses args)
  | Jump {args, ...} =>
    List.foldl
	(fn (SOME id, set) => add (set, id) | (NONE, set) => set)
	empty (vec Atom.uses args)
  | Switch {test, ...} =>
    (case Atom.uses test of SOME id => singleton id | NONE => empty)
  | Return atom =>
    (case Atom.uses atom of SOME id => singleton id | NONE => empty)
end
end

structure Block = struct
datatype t = datatype block
datatype control = datatype control
datatype statement = datatype statement

structure Uses = Uses
structure Free = Uses
structure Defs = Defs

fun foldParams f id (Block {params, ...}) = 
    Vector.foldr f id params

fun foldBody f id (Block {body, ...}) =
    Vector.foldr f id body

fun name (Block {label, ...}) = label

local open Defs in
fun defs block
  = foldBody
	(fn (stm, set) =>
	    case Statement.defs stm
	     of SOME dest => add (set, dest)
	      | NONE => set)
	(foldParams add' empty block) 
	block
end

local open Uses in
fun uses (block as Block {transfer, ...})
  = foldBody
	(fn (stm, set) => union (set, (Statement.uses stm)))
	(Control.uses transfer)
	block
end

local open Free in
fun free block =
    let 
	val uses' = uses block 
	val defs' = Defs.foldl (fn ((id,_),set) => add (set, id)) empty (defs block) 
    in
	difference (uses', defs')
    end
end


end

structure Function = struct
structure Locals = Uses

datatype t = datatype function
datatype block = datatype block

fun name (Function {name, ...}) = name

fun foldBlocks f id (Function {blocks, ...}) =
    Vector.foldr f id blocks

fun findBlock p (Function {blocks, ...}) =
    Vector.find p blocks

fun foldParams f id (Function {params, ...}) = 
    Vector.foldr f id params

fun startBlock (function as Function {start, ...}) =
    valOf (findBlock (fn (Block {label,...}) => start = label) function)
    handle Option => raise Fail ("Malformed function: no block named "^start)

fun locals function =
    (* Determine which variables are local by subtracting the
     * function's parameters from the union of all blocks' free variables. *)
    let
	val free =
	    foldBlocks 
		(fn (b, set) => Locals.union (set, Block.free b))
		Locals.empty function
	val params =
	    foldParams
		(fn ((id,_), set) => Locals.add (set, id))
		Locals.empty function
    in
	Locals.difference (free, params)
    end

end

structure Program = struct
datatype t = datatype program
datatype function = datatype function

fun foldFunctions f id (Program {functions, ...}) =
    Vector.foldr f id functions

fun foldGlobals f id (Program {globals, ...}) = 
    Vector.foldr (fn ((id,t),i) => f (id,t,i)) id globals

fun foldTypes f id (Program {types, ...}) =
    Vector.foldr f id types

end

end
