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

    datatype immediate
    (* A literal value *)
      = Real of string
      | Int of string
      | Bool of string
      | String of string
      | Const of ident
      | Infinity
      | Nan
      | Undefined

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
	type expression
	datatype t
	  = Int_add
	  | Int_sub
	  | Int_mul
	  | Int_lt
	  (* | ... *)

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
	  (* | ... *)

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

	  | Vector_extract
	  (* | ... *)

	  | Array_array
	  | Array_extract
	  (* | ... *)

	  | Matrix_dense
	  | Matrix_banded

	  | Record_record
	  | Record_extract
	  (* | ... *)

	  | Random_uniform
	  | Random_normal

	  | Address_addr
	  | Address_deref

	  | Sim_if
	  | Sim_input
	  | Sim_output
	  | Sim_bug

	val name: t -> string
	val find: string -> t option

	structure Record: sig
	    val extract: expression * ident -> expression
	end
	structure Array: sig
	    val extract: expression * expression -> expression
	end
	structure Address: sig
	    val addr: expression -> expression
	    val deref: expression -> expression
	end

    end

    structure Atom: sig
	datatype t
	  = Null
	  | Void
	  | Literal of immediate
	  | Source of t
	  | Sink of t
	  | Variable of ident
	  | Symbol of ident
	  | RuntimeVar of ((unit -> t) * Type.t)
	  | CompileVar of ((unit -> t) * Type.t)
	  | Address of address
	  | Label of ident
	  | Cast of t * Type.t
    end

    structure Expression: sig
	type atom
	type operator
	datatype t
	  = Value of atom
	  | Apply of {oper: operator,
		      args: t vector}

	val var: ident -> t
    end
    sharing type Expression.atom = Atom.t
    sharing type Expression.operator = Operator.t
    sharing type Expression.t = Operator.expression

    structure Statement: sig
	type atom
	type operator
	type expression
	datatype t
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
	  | Move of {src: atom, dest: atom}
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
	  = Call of {func: ident,
		     args: atom vector, 
		     return: t option}
	  (* "Goto-with-arguments" transfers control to a labeled
	   * block. This makes variable versioning explicit without the
	   * need for SSA-style "phi" functions *)
	  | Jump of {block: label,
		     args: atom vector}

	  (* Conditional branching to a labeled block. A given
	   * atom is compared against a sequence of constants. If
	   * an equal constant is found, control transfers to the
	   * block having the corresponding label. If none match,
	   * control transfers to a default labeled block. The
	   * target block must be nullary. *)
	  | Switch of {test: atom,
		       cases: (immediate * t) vector,
		       default: t}
	  (* Exits the current calling context and passes the result value to the return continuation label. *)
	  | Return of atom
    end
    sharing type Control.atom = Atom.t

    structure Block: sig
	structure Uses: sig
	    (* A uses set comprises all variable identifiers
	     * appearing in a block. *)
	    include ORD_SET where type Key.ord_key = ident
	end

	structure Defs: sig
	    (* A defs set comprises all variable definitions
	     * appearing in a block, including parameters
	     * and bindings. *)
	    include ORD_SET where type Key.ord_key = ident * Type.t
	end

	structure Free: sig
	    (* A free set comprises all variable identifers
	     * appearing in a block, excluding definitions. *)
	    include ORD_SET where type Key.ord_key = ident
	end

	type statement
	(* A basic block is a labeled sequence of statements
	 * with a terminating control flow operation. Blocks may
	 * accept an arbitrary number of parameters. *)
	datatype t
	  = Block of {label: label,
		      params: (ident * Type.t) vector,
		      body: Statement.t vector,
		      transfer: Control.t}

	val name: t -> string

	val uses: t -> Uses.set
	val defs: t -> Defs.set
	val free: t -> Free.set

	val foldParams: ((ident * Type.t) * 'a -> 'a) -> 'a -> t -> 'a
	val foldBody: (Statement.t * 'a -> 'a) -> 'a -> t -> 'a
    end
    sharing type Block.statement = Statement.t
    
    structure Function: sig
	structure Locals: sig
	    (* A locals set comprises all variable identifers
	     * appearing free in blocks, excluding function parameters. *)
	    include ORD_SET where type Key.ord_key = ident
	end

	type block
	datatype t
	  = Function of {params: (ident * Type.t) vector,
			 name: ident,
			 start: label,
			 blocks: Block.t vector,
			 returns: Type.t}

	val name: t -> string

	val locals: t -> Locals.set

	val foldParams: ((ident * Type.t) * 'a -> 'a) -> 'a -> t -> 'a

	val foldBlocks: (block * 'a -> 'a) -> 'a -> t -> 'a
	val findBlock: (block -> bool) -> t -> block option

	val startBlock: t -> block
    end
    sharing type Function.block = Block.t

    structure Program: sig
	type function
	datatype t 
	  = Program of {functions: function vector,
			main: function,
			globals: (ident * Type.t) vector,
			types: TypeDeclaration.t vector}

	val foldFunctions: (function * 'a -> 'a) -> 'a -> t -> 'a
	val foldGlobals: (ident * Type.t * 'a -> 'a) -> 'a -> t -> 'a
	val foldTypes: (TypeDeclaration.t * 'a -> 'a) -> 'a -> t -> 'a
    end
    sharing type Program.function = Function.t

    structure Context: sig
	(* An ordered list of variable declarations. *)
	type context
	type atom
	type expression
	type operator
	datatype binding
	  = Assumption of Type.t
	  | Bind of atom * Type.t
	  | Graph of expression * Type.t
	  | Primitive of operator * Type.t

	val empty: context
	val add: context * ident * binding -> context
	val find: context * ident -> binding option
    end
    sharing type Context.atom = Atom.t
    sharing type Context.operator = Operator.t
    sharing type Context.expression = Expression.t

    structure Environment: sig
	(* An ordered list of global declarations. *)
	type env
	type expression
	type context
	datatype binding
	  = Assumption of Type.t
	  | Function of Type.t
	  | Global of expression * Type.t
	  | Type of Type.t

	val empty: env
	val add: env * context * ident * binding -> env
    end
    sharing type Environment.context = Context.context
    sharing type Environment.expression = Expression.t


    datatype fragment
       = ATOM of Atom.t
       | OPERATOR of Operator.t
       | EXPRESSION of Expression.t
       | STATEMENT of Statement.t
       | CONTROL of Control.t
       | BLOCK of Block.t
       | FUNCTION of Function.t
       | PROGRAM of Program.t
       | TYPENAME of Type.t
       | SERIES of fragment vector (* Comma-delimited *)
       | SEQUENCE of fragment vector (* Semicolon-delimited *)
end

