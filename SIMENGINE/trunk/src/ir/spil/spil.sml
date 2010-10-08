structure Spil: SPIL = struct

type size = int
type ident = string
type label = string

structure Uses = BinarySetFn(type ord_key = ident val compare = String.compare)
structure Defs = BinarySetFn(type ord_key = ident * Type.t val compare = fn ((a,_),(b,_)) => String.compare (a,b))


fun vec f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i)))


datatype immediate 
  = Real of real
  | Int of int
  | Bool of bool
  | String of string
  | Const of ident
  | Nan
  | Infinity

type address = string

structure TypeDeclaration = struct
datatype t 
  = ARRAY of {name: ident,
	      size: size,
	      base: Type.t}
  | STRUCTURE of {name: ident,
		  fields: (ident * Type.t) vector}
end



datatype atom
  = Null
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
  | Offset of
    {base: atom, index: size, offset: size, scale: size, basetype: Type.t}
  | Offset2D of
    {base: atom, 
     index: {x:size, y:size}, 
     offset: {x:size, y:size},
     scale: {x:size, y:size},
     basetype: Type.t}
  | Offset3D of
    {base: atom,
     index: {x:size, y:size, z:size},
     offset: {x:size, y:size, z:size},
     scale: {x:size, y:size, z:size},
     basetype: Type.t}

     and operator
       = Int_add
       | Int_sub
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
       | Array_insert
       | Vector_extract
       | Vector_insert
       | Matrix_dense
       | Matrix_banded
       | Record_record
       | Record_extract
       | Record_insert
       | Random_uniform
       | Random_normal
       | Cell_ref
       | Cell_deref
       | Sim_if
       | Sim_input
       | Sim_output
       | Sim_bug

     and expression
       = Value of atom
       | Apply of {oper: operator,
		   args: expression vector}

     and statement
       = HALT
       | NOP
       | COMMENT of string
       | PROFILE of ident
       | BIND of {src: atom,
		  dest: ident * Type.t}
       | GRAPH of {src: expression,
		   dest: ident * Type.t}
       | PRIMITIVE of {oper: operator,
		       args: atom vector,
		       dest: ident * Type.t}
       | MOVE of {src: atom,
		  dest: atom}

     and control
       = CALL of {func: ident,
		  args: atom vector, 
		  return: label option} (* NONE for tail calls *)
       | JUMP of {block: label,
		  args: atom vector}
       | SWITCH of {test: atom,
		    cases: (immediate * label) vector,
		    default: label}
       | RETURN of atom

     and block
       = BLOCK of {label: label,
		   params: (ident * Type.t) vector,
		   body: statement vector,
		   transfer: control}

     and function
       = FUNCTION of {params: (ident * Type.t) vector,
		      name: ident,
		      start: label,
		      blocks: block vector,
		      returns: Type.t}

     and program
       = PROGRAM of {functions: function vector,
		     main: function,
		     globals: (ident * Type.t) vector,
		     types: TypeDeclaration.t vector}




structure Atom = struct
type context = unit
datatype t = datatype atom

val rec uses =
 fn Variable id => SOME id
  | Cast (atom, t) => uses atom
  | Offset {base, ...} => uses base
  | Offset2D {base, ...} => uses base
  | Offset3D {base, ...} => uses base
  | Source base => uses base
  | Sink base => uses base
  | RuntimeVar (f, t) => uses (f ())
  | CompileVar (f, t) => uses (f ())
  | _ => NONE
end

structure Operator = struct
datatype t = datatype operator

val name =
 fn Int_add => "Int_add"
  | Int_sub => "Int_sub"
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
  | Array_insert => "Array_insert"
  | Vector_extract => "Vector_extract"
  | Vector_insert => "Vector_insert"
  | Matrix_dense => "Matrix_dense"
  | Matrix_banded => "Matrix_banded"
  | Record_record => "Record_record"
  | Record_extract => "Record_extract"
  | Record_insert => "Record_insert"
  | Random_uniform => "Random_uniform"
  | Random_normal => "Random_normal"
  | Cell_ref => "Cell_ref"
  | Cell_deref => "Cell_deref"
  | Sim_if => "Sim_if"
  | Sim_input => "Sim_input"
  | Sim_output => "Sim_output"
  | Sim_bug => "Sim_bug"

end

structure Expression = struct
datatype t = datatype expression
datatype atom = datatype atom
datatype operator = datatype operator

local open Uses in
val rec uses =
 fn Apply {oper, args} => 
    List.foldl union empty (vec uses args)
  | Value atom => 
    (case Atom.uses atom of SOME id => singleton id | NONE => empty)
end
end

structure Statement = struct
type context = unit
datatype t = datatype statement
datatype atom = datatype atom
datatype operator = datatype operator
datatype expression = datatype expression

val defs =
 fn HALT => NONE
  | NOP => NONE
  | COMMENT _ => NONE
  | PROFILE _ => NONE
  | BIND {src, dest} => SOME dest
  | GRAPH {src, dest} => SOME dest
  | PRIMITIVE {oper, args, dest} => SOME dest
  | MOVE {src, dest} => NONE

local open Uses in
val uses =
 fn HALT => empty
  | NOP => empty
  | COMMENT _ => empty
  | PROFILE _ => empty
  | BIND {src, dest} =>
    (case Atom.uses src of SOME id => singleton id | NONE => empty)
  | GRAPH {src, dest} => Expression.uses src
  | PRIMITIVE {oper, args, dest} => 
    List.foldl
	(fn (SOME id, set) => add (set, id) | (NONE, set) => set)
	empty (vec Atom.uses args)
  | MOVE {src, dest} => 
    addList (empty, List.mapPartial (fn x => x) [Atom.uses src, Atom.uses dest])
end
end

structure Control = struct
datatype t = datatype control
datatype atom = datatype atom
end

structure Block = struct
datatype t = datatype block
datatype control = datatype control
datatype statement = datatype statement

structure Uses = Uses
structure Free = Uses
structure Defs = Defs

fun foldParams f id (BLOCK {params, ...}) = 
    Vector.foldr f id params

fun foldBody f id (BLOCK {body, ...}) =
    Vector.foldr f id body

fun name (BLOCK {label, ...}) = label

local open Defs in
fun defs block 
  = foldBody
	(fn (stm, set) =>
	    case Statement.defs stm
	     of SOME dest => add (set, dest)
	      | NONE => set)
	(foldParams add' empty block) block
end

local open Uses in
fun uses block
  = foldBody
	(fn (stm, set) => union (set, (Statement.uses stm)))
	empty block
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
datatype t = datatype function
datatype block = datatype block

fun foldBlocks f id (FUNCTION {blocks, ...}) =
    Vector.foldr f id blocks

fun foldParams f id (FUNCTION {params, ...}) = 
    Vector.foldr f id params

fun startBlock (FUNCTION {start, blocks, ...}) =
    valOf (Vector.find (fn (BLOCK {label,...}) => start = label) blocks)

end

structure Program = struct
datatype t = datatype program
datatype function = datatype function

fun foldFunctions f id (PROGRAM {functions, ...}) =
    Vector.foldr f id functions

fun foldGlobals f id (PROGRAM {globals, ...}) = 
    Vector.foldr (fn ((id,t),i) => f (id,t,i)) id globals

fun foldTypes f id (PROGRAM {types, ...}) =
    Vector.foldr f id types

end

end
