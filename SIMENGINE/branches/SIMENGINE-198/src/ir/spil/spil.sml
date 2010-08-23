structure Spil: SPIL = struct



type size = int
type ident = string
type label = string
type immediate = unit
type address = int

structure Type = struct
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
  | Variable of ident
  | Address of address
  | Label of ident
  | Literal of immediate
  | Offset of {base: atom, offset: size, scale: size}

     and operator
       = Int_add
       | Int_sub
       | Float_add
       | Float_sub
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
       = FUNCTION of {args: (ident * Type.t) vector,
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
datatype t = datatype atom
end

structure Operator = struct
datatype t = datatype operator
end

structure Expression = struct
datatype t = datatype expression
datatype atom = datatype atom
datatype operator = datatype operator
end

structure Statement = struct
datatype t = datatype statement
datatype atom = datatype atom
datatype operator = datatype operator
datatype expression = datatype expression
end

structure Control = struct
datatype t = datatype control
datatype atom = datatype atom
end

structure Block = struct
datatype t = datatype block
datatype control = datatype control
datatype statement = datatype statement
end

structure Function = struct
datatype t = datatype function
datatype block = datatype block
end

structure Program = struct
datatype t = datatype program
datatype function = datatype function
end

end
