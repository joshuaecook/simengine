(* SimEngine Programmatic Intermediate Language
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
    type size
    (* A variable identifier *)
    type ident
    (* A program label *)
    type label
    (* A literal value *)
    type immediate
    type address

    structure Type: sig
	datatype t
	  = PRIMITIVE of primitive_t
	  | VECTOR of primitive_t * size
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

    structure Operator: sig
	datatype t
	  = Int_add
	  | Int_sub
	  (* | ... *)

	  | Float_add
	  | Float_sub
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
    end

    structure Atom: sig
	datatype t
	  = Null
	  | Variable of ident
	  | Address of address
	  | Label of ident
	  | Literal of immediate
	  | Offset of {base: t, offset: size, scale: size}
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
	  | MOVE of {src: atom,
		     dest: atom}
    end
    sharing type Statement.atom = Atom.t
    sharing type Statement.operator = Operator.t
    sharing type Statement.expression = Expression.t

    structure Control: sig
	type atom
	datatype t
	  = CALL of {func: ident,
		     args: atom vector, 
		     return: label option} (* NONE for tail calls *)
	  | JUMP of {block: label,
		     args: atom vector}
	  | SWITCH of {test: atom,
		       cases: (immediate * label) vector,
		       default: label}
	  | RETURN of atom vector
    end
    sharing type Control.atom = Atom.t

    structure Block: sig
	type statement
	datatype t
	  = BLOCK of {label: label,
		      args: (ident * Type.t) vector,
		      body: Statement.t vector,
		      transfer: Control.t}
    end
    sharing type Block.statement = Statement.t
    
    structure Function: sig
	type block
	datatype t
	  = FUNCTION of {args: (ident * Type.t) vector,
			 name: ident,
			 start: label,
			 blocks: Block.t vector,
			 returns: Type.t}
    end
    sharing type Function.block = Block.t

    structure Program: sig
	type function
	datatype t 
	  = PROGRAM of {functions: function vector,
			main: function,
			types: TypeDeclaration.t vector}
    end
    sharing type Program.function = Function.t
end

(*

local 
    open SPIL
    structure Stm = Statement
    structure Ctl = Control
    structure Op = Operator
    structure A = Atom
    structure B = Block
    structure F = Function
    structure Pro = Program

    val CDATAFORMAT = Type.PRIMITIVE (Float 4)
in

B.BLOCK
    {label = "fn_u",
     args = #[],
     body = 
     #[Stm.COMMENT "(1) u' = u + (u^3)/3 + I",
       Stm.BIND {dest = ("y",CDATAFORMAT), 
		 src = A.Address "fn_states"},
       Stm.BIND {dest = ("u",CDATAFORMAT), 
		 src = A.Offset {base=A.Variable "y",
			       offset=U_OFF,
			       scale=1}},
       Stm.BIND {dest = ("I",CDATAFORMAT), 
		 src = A.Offset {base=A.Address "fn_inputs",
			       offset=I_OFF,
			       scale=1}},
       Stm.BIND {dest = ("dydt",CDATAFORMAT), 
		 src = A.Address "fn_next_states"},
       Stm.GRAPH {dest = ("dudt",CDATAFORMAT)
		  src = 
		  Exp.APPLY 
		      {oper = Float_add,
		       args = 
		       #[Exp.VALUE (A.Variable "u"),
			 Exp.APPLY
			     {oper = Float_mul,
			      args = 
			      #[Exp.VALUE (A.Variable "u"),
				Exp.VALUE (A.Variable "u"),
				Exp.VALUE (A.Variable "u"),
				Exp.VALUE (A.Literal (1/3))]},
			 Exp.VALUE (A.Variable "I")]}},
       Stm.MOVE {dest = A.Offset {base=A.Variable "dydt",
				  offset=U_OFF,
				  scale=1},
		 src = A.Variable "dudt"}
     ],
     transfer = Ctl.JUMP {dest="fn_w",
			  args=#[]}
    }
;
B.BLOCK
    {label = "fn_w",
     args = #[],
     body =
     #[Stm.COMMENT "(2) w' = e * (b0 + b1 * u - w)",
       Stm.BIND {dest= ("y", CDATAFORMAT), 
		 src = A.Address "fn_states"},
       Stm.BIND {dest = ("u", CDATAFORMAT),
		 src = A.Offset {base=A.Variable "y",
			       offset=u_OFF,
			       scale=1}},
       Stm.BIND {dest = ("w", CDATAFORMAT),
		 src = A.Offset {base=A.Variable "y",
			       offset=w_OFF,
			       scale=1}},
       Stm.BIND {dest = ("e", CDATAFORMAT),
		 src = A.Offset {base=A.Address "fn_inputs",
			       offset=e_OFF,
			       scale=1}},
       Stm.BIND {dest = ("b0", CDATAFORMAT),
		 src = A.Offset {base=A.Address "fn_inputs",
			       offset=b0_OFF,
			       scale=1}},
       Stm.BIND {dest = ("b1", CDATAFORMAT),
		 src = A.Offset {base=A.Address "fn_inputs",
			       offset=b1_OFF,
			       scale=1}},
       Stm.BIND {dest = ("dydt", CDATAFORMAT),
		 src = A.Address "fn_next_states"},
       Stm.GRAPH {dest = ("dwdt", CDATAFORMAT),
		  src = 
		  Exp.APPLY
		      {oper = Float_mul,
		       args =
		       #[Exp.VALUE (A.Variable "e"),
			 Exp.APPLY 
			     {oper = Float_add,
			      args = 
			      #[Exp.VALUE (A.Variable "b0"),
				Exp.APPLY 
				    {oper = Float_mul,
				     args =
				     #[Exp.VALUE (A.Variable "b1"),
				       Exp.VALUE (A.Variable "u")]},
				Exp.APPLY 
				    {oper = Float_neg,
				     args = #[Exp.VALUE (A.Variable "w")]}]}]}},
       Stm.MOVE {dest = A.Offset {base=A.Variable "dydt",
				  offset=W_OFF,
				  scale=1},
		 src = A.Variable "dwdt"}
       
     ],
     transfer = Ctl.RETURN #[]
    }
;

F.FUNCTION
    {args = #[],
     name = "fn",
     start = "fn_u",
     blocks = #[fn_u,fn_w],
     returns = Void
    }

;

B.BLOCK
    {label = "main_entry",
     args = #[],
     body = #[],
     transfer = Ctl.CALL {func = "fn",
			  args = #[],
			  return = NONE}
    }

;

F.FUNCTION
    {args = #[],
     name = "main",
     start = "main_entry"
     blocks = #[main_entry],
     returns = Void
    }

;

Pro.PROGRAM
    {functions = #[fn],
     main = main,
     types = #[...]     
    }


end









*)

