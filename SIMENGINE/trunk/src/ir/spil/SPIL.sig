(* SimEngine Programmatic Intermediate Language
 * Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. 
 *
 * SPIL is a strict, explicitly-(simply-)typed, first-order,
 * functional intermediate form, similar to SSA, influenced by the
 * MLton intermediate languages, the LLVM assemly language, and the
 * PTX instruction set architecture.  Programs written in SPIL are
 * representations of a sequential, calculating state machine.
 * 
 * Goals 
 *
 * The language should express the vector nature of the underlying
 * machine. It should provide structured data type abstraction,
 * subroutine definition, and control transfer along with basic
 * calculating primitives and special operators.
 *
 * Programs should be serializable to a format that can be recovered
 * to an equivalent program.
 *
 * Programming environment
 * 
 * A program executes at a particular point within a multi-dimensional
 * space of identical programs. The program's coordinates within this
 * space are available as global constants, and the program's
 * behaviour may depend on its coordinates.
 *
 * Programs within the same first-dimension space may communicate via
 * shared memory. The address of the shared memory space is available
 * as a global constant.
 *
 * A program executes within a runtime environment which manages
 * memory and I/O and configures the program topology.
 *     
 * Validity
 * 
 * A valid program may not contain duplicate identifiers.
 * Variable definitions must dominate their uses.
 * 
 * References
 * http://mlton.org/pipermail/mlton/2007-February/029597.html
 * http://mlton.org/pipermail/mlton/2001-August/019689.html
 * http://mlton.org/SSA
 * http://llvm.org/docs/LangRef.html
 * http://developer.download.nvidia.com/compute/cuda/3_1/toolkit/docs/ptx_isa_2.1.pdf
 * Modern Compiler Implementation in ML. (Appel)
 *)

signature SPIL = sig
    (* The dimensionality of a datum *)
    type size = int

    (* A variable identifier *)
    type ident = string

    (* A program label *)
    type label = string

    datatype immediate =
    (* A literal value *)
	     Real of real
	   | Int of int

    type address = string

    structure TypeDeclaration: sig
	datatype t 
	  = ARRAY of {name: ident,
		      size: size,
		      base: Type.t}
	  | STRUCTURE of {name: ident,
			  fields: (ident * Type.t) vector}
    end

    structure Operator: sig
	datatype t
	  = Int_add
	  | Int_sub
	  (* | ... *)

	  | Float_add
	  | Float_sub
	  | Float_mul
	  | Float_neg
	  (* | ... *)

	  | Vector_extract
	  | Vector_insert
	  (* | ... *)

	  | Array_extract
	  | Array_insert
	  (* | ... *)

	  | Structure_extract
	  | Structure_insert
	  (* | ... *)

	val name: t -> string
    end

    structure Atom: sig
	datatype t
	  = Null

	  | Literal of immediate
	  | Variable of ident
	  | RuntimeVar of ident
	  | CompileVar of ident
	  | Address of address
	  | Label of ident

	  | Offset of
	    {base: t, index: size, offset: size, scale: size, basetype: Type.t}
          (*
	   #define OFFSET (b,i,o,s,t) \
		   (*((t)*)((b) + ((i)*(s)) + (o)))
	   *)
	  | Offset2D of
	    {base: t, 
	     index: {x:size, y:size}, 
	     offset: {x:size, y:size},
	     scale: {x:size, y:size},
	     basetype: Type.t}
	  | Offset3D of
	    {base: t,
	     index: {x:size, y:size, z:size},
	     offset: {x:size, y:size, z:size},
	     scal: {x:size, y:size, z:size},
	     basetype: Type.t}

    end

    structure Expression: sig
	type atom
	type operator
	datatype t
	  = Value of atom
	  | Apply of {oper: operator,
		      args: t vector}
    end
    sharing type Expression.atom = Atom.t
    sharing type Expression.operator = Operator.t

    structure Statement: sig
	type atom
	type operator
	type expression
	datatype t
	  = HALT
	  | NOP
	  | COMMENT of string
	  | PROFILE of ident
	  | LABEL of ident
	  | BIND of {src: atom,
		     dest: ident * Type.t}
	  | GRAPH of {src: expression,
		      dest: ident * Type.t}
	  | PRIMITIVE of {oper: operator,
			  args: atom vector,
			  dest: ident * Type.t}
	  | MOVE of {src: atom, dest: atom}
	  | CAST of {src: atom, dest: atom * Type.t}
    end
    sharing type Statement.atom = Atom.t
    sharing type Statement.operator = Operator.t
    sharing type Statement.expression = Expression.t

    structure Control: sig
	type atom
	datatype t
	  (* Invoke a named function with an optional return
	   * continuation label. An absent return label indicates
	   * a tail-position call. The continuation block must be unary.
	   * Implementation note: the CALL control enables recursion
	   * via tail position calls or non-tail calls. Tail call
	   * recursion may be optimized to prevent stack
	   * growth. Stackless platforms may reject programs having
	   * non-tail calls. *)
	  = CALL of {func: ident,
		     args: atom vector, 
		     return: label option}
	  (* "Goto-with-arguments" transfers control to a labeled
	   * block. This makes variable versioning explicit without the
	   * need for SSA-style "phi" functions *)
	  | JUMP of {block: label,
		     args: atom vector}

	  (* Conditional branching to a labeled block. A given
	   * atom is compared against a sequence of constants. If
	   * an equal constant is found, control transfers to the
	   * block having the corresponding label. If none match,
	   * control transfers to a default labeled block. The
	   * target block must be nullary. *)
	  | SWITCH of {test: atom,
		       cases: (immediate * label) vector,
		       default: label}
	  (* Exits the current calling context and passes the result value to the return continuation label. *)
	  | RETURN of atom
    end
    sharing type Control.atom = Atom.t

    structure Block: sig
	type statement
	(* A basic block is a labeled sequence of statements
	 * with a terminating control flow operation. Blocks may
	 * accept an arbitrary number of parameters. *)
	datatype t
	  = BLOCK of {label: label,
		      params: (ident * Type.t) vector,
		      body: Statement.t vector,
		      transfer: Control.t}

	val name: t -> string

	val uses:
	    (* The list of used variables. *)
	    t -> ident list

	val defs: 
	    (* The list of defined variables. *)
	    t -> (ident * Type.t) list

	val foldParams: (ident * Type.t * 'a -> 'a) -> 'a -> t -> 'a
	val foldBody: (Statement.t * 'a -> 'a) -> 'a -> t -> 'a
    end
    sharing type Block.statement = Statement.t
    
    structure Function: sig
	type block
	datatype t
	  = FUNCTION of {params: (ident * Type.t) vector,
			 name: ident,
			 start: label,
			 blocks: Block.t vector,
			 returns: Type.t}

	val foldBlocks: (block * 'a -> 'a) -> 'a -> t -> 'a
	val foldParams: (ident * Type.t * 'a -> 'a) -> 'a -> t -> 'a

	val startBlock: t -> block
    end
    sharing type Function.block = Block.t

    structure Program: sig
	type function
	datatype t 
	  = PROGRAM of {functions: function vector,
			main: function,
			globals: (ident * Type.t) vector,
			types: TypeDeclaration.t vector}

	val foldFunctions: (function * 'a -> 'a) -> 'a -> t -> 'a
	val foldGlobals: (ident * Type.t * 'a -> 'a) -> 'a -> t -> 'a
	val foldTypes: (TypeDeclaration.t * 'a -> 'a) -> 'a -> t -> 'a
    end
    sharing type Program.function = Function.t
end

