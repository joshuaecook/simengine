structure Spil: SPIL = struct

type size = int
type ident = string
type label = string

datatype immediate 
  = Real of real
  | Int of int
  | Bool of bool

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
  | RuntimeVar of ((unit -> atom) * Type.t)
  | CompileVar of ((unit -> atom) * Type.t)
  | Address of address
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
       | Vector_extract
       | Vector_insert
       | Array_extract
       | Array_insert
       | Structure_extract
       | Structure_insert

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

local structure T = Type in
fun typeof atom cxt =
    case atom
     of Null => (T.gen T.void, cxt)
      | Literal (Bool _) => (T.gen T.bool, cxt)
      | Literal (Real _) => (T.gen (T.real 64), cxt)
      | Literal (Int _) => (T.gen (T.int 32), cxt)
      | RuntimeVar (_, t) => (t, cxt)
      | CompileVar (_, t) => (t, cxt)
      | Variable _ => raise Fail "lookup from context"
      | Cast (_,t) => (t, raise Fail "add to context")
      | Offset {basetype,...} => (basetype, cxt)
      | Offset2D {basetype,...} => (basetype, cxt)
      | Offset3D {basetype,...} => (basetype, cxt)

end
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
  | Vector_extract => "Vector_extract"
  | Vector_insert => "Vector_insert"
  | Array_extract => "Array_extract"
  | Array_insert => "Array_insert"
  | Structure_extract => "Structure_extract"
  | Structure_insert => "Structure_insert"

end

structure Expression = struct
datatype t = datatype expression
datatype atom = datatype atom
datatype operator = datatype operator
end

structure Statement = struct
type context = unit
datatype t = datatype statement
datatype atom = datatype atom
datatype operator = datatype operator
datatype expression = datatype expression

fun bind atom (cxt, (id,t)) 
  = (BIND {src= atom, dest= (id,t)}, cxt)

fun bindExp exp (cxt, (id,t)) 
  = (GRAPH {src= exp, dest= (id,t)}, cxt)

fun bindPrim (oper,args) (cxt, (id,t))
  = (PRIMITIVE {oper= oper, args= args, dest= (id,t)}, cxt)

val comment = COMMENT
val profile = PROFILE
val move = MOVE
val halt = HALT
val nop = NOP

end

structure Control = struct
datatype t = datatype control
datatype atom = datatype atom
end

structure Block = struct
datatype t = datatype block
datatype control = datatype control
datatype statement = datatype statement

fun foldParams f id (BLOCK {params, ...}) = 
    Vector.foldr (fn ((id,t),i) => f (id,t,i)) id params

fun foldBody f id (BLOCK {body, ...}) =
    Vector.foldr f id body

fun name (BLOCK {label, ...}) = label

fun defs block 
  = foldParams
	(fn (id,t,acc) => (id,t)::acc)
	(foldBody
	     (fn (stm, acc) =>
		 case stm
		  of BIND {dest, ...} => dest::acc
		   | GRAPH {dest, ...} => dest::acc
		   | PRIMITIVE {dest, ...} => dest::acc
		   | _ => acc)
	     nil block)
	block

fun uses block
  = foldBody
	(fn (stm, acc) => acc)
	nil block
    


end

structure Function = struct
datatype t = datatype function
datatype block = datatype block

fun foldBlocks f id (FUNCTION {blocks, ...}) =
    Vector.foldr f id blocks

fun foldParams f id (FUNCTION {params, ...}) = 
    Vector.foldr (fn ((id,t),i) => f (id,t,i)) id params

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
