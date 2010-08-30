(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

signature SPIL_TO_SCALAR = sig
    structure Spil: SPIL

    val spilToScalar: Spil.Program.t -> Layout.t
end


functor SpilToScalar (T: SPIL): SPIL_TO_SCALAR = struct

structure Spil = T

structure Layout = struct
open Layout

val comment =
 fn nil => empty
  | [line] => seq [str "/* ", str line, str " */"]
  | line::more => align [str "/* ", str line,
			 alignPrefix (map str more, " * "),
			 str " */"]

fun stm line = seq [line, str ";"]

fun vec f v = f (List.tabulate (Vector.length v, fn i => Vector.sub (v,i)))

fun sub t = indent (t,4)

end
open Layout



fun spilToScalar (program)
  = align [comment ["Copyright (c) 2010 by Simatra Modeling Technologies, L.L.C"],
	   typeDeclarations program,
	   globalDefinitions program,
	   functionPrototypes program,
	   functionDefinitions program,
	   mainDefinition program,
	   comment ["End of program"]]

and typeDeclarations (program)
  = comment ["= Type Declarations ="]

and globalDefinitions (program)
  = comment ["= Global Variable Definitions ="]

and functionPrototypes (program)
  = align [comment ["= Function Prototypes ="],
	   align
	       (Spil.Program.foldFunctions
		    (fn (f,lyt) => (stm (layoutFunctionPrototype f))::lyt) 
		    nil program)]

and layoutFunctionPrototype (f as Spil.Function.FUNCTION {name, returns, ...}) 
  = space [layoutType returns, str name, layoutFunctionParams f]

and layoutType t
  = str (Type.toString t)

and layoutFunctionParams (function)
  = tuple (Spil.Function.foldParams
	       (fn (id,t,lyt) => (seq [layoutType t, str id])::lyt)
	       nil function)

and functionDefinitions (program)
  = align [comment ["= Function Definitions ="],
	   align 
	       (Spil.Program.foldFunctions
		    (fn (f,lyt) => (layoutFunctionDefinition f)::lyt)
		    nil program)]

and layoutFunctionDefinition (f as Spil.Function.FUNCTION {name, ...})
  = align [comment ["Function " ^ name],
	   blockPrototypes f,
	   blockDefinitions f,
	   layoutFunctionPrototype f,
	   str "{",
	   sub(layoutFunctionBody f),
	   str "}"]

and layoutFunctionBody (f as Spil.Function.FUNCTION {start, ...})
  = stm (space [str start, str "()"])
    
and layoutBlockPrototype (b as Spil.Block.BLOCK {label, ...})
  = space [str "void", str label, layoutBlockParams b]

and layoutBlockParams (function)
  = tuple (Spil.Block.foldParams
	       (fn (id,t,lyt) => (seq [layoutType t, str id])::lyt)
	       nil function)

and blockPrototypes (function)
  = align
	(Spil.Function.foldBlocks
	     (fn (b,lyt) => (stm (layoutBlockPrototype b))::lyt) 
	     nil function)

and blockDefinitions (function)
  = align
	(Spil.Function.foldBlocks
	     (fn (b,lyt) => (layoutBlockDefinition b)::lyt) 
	     nil function)

and layoutBlockDefinition (b as Spil.Block.BLOCK {label, ...})
  = align [comment ["Block " ^ label],
	   layoutBlockPrototype b,
	   str "{",
	   layoutStatement (Spil.Statement.LABEL label),
	   sub(align(Spil.Block.foldBody
			 (fn (s,lyt) => (layoutStatement s)::lyt)
			 nil b)),
	   sub(layoutTransfer b),
	   str "}"]

and layoutStatement (statement)
  = case statement
     of Spil.Statement.HALT 
	=> stm (str "exit(1)")

      | Spil.Statement.NOP
	=> stm (str "")

      | Spil.Statement.COMMENT text
	=> comment [text]

      | Spil.Statement.LABEL ident
	=> (seq [str ident, str ":"])

      | Spil.Statement.BIND {src, dest}
	=> bindExpression (dest, Spil.Expression.Value src)

      | Spil.Statement.GRAPH {src, dest}
	=> bindExpression (dest, src)

      | Spil.Statement.PRIMITIVE _
	=> stm (str "PRIMITIVE")

      | Spil.Statement.MOVE {src, dest} =>
	stm (space [layoutAtom dest, str "=", layoutAtom src])

      | Spil.Statement.PROFILE _ 
	=> stm (str "PROFILE")

and bindExpression ((id,t), src) =
    let
	val dest = space [layoutType t, str id]
    in
	stm (space [dest, str "=", layoutExpression src])
    end

and layoutExpression (exp)
  = case exp
     of Spil.Expression.Value atom => layoutAtom atom
      | Spil.Expression.Apply {oper, args}
	=> seq [str (Spil.Operator.name oper),
		tuple (vec (fn x => x) (Vector.map layoutExpression args))]

and layoutAtom atom
  = case atom
     of Spil.Atom.Null => str "0"
      | Spil.Atom.Variable id => str id
      | Spil.Atom.Address addr => str addr
      | Spil.Atom.Label id => str id
      | Spil.Atom.Literal lit => layoutLiteral lit
      | Spil.Atom.Offset {base,offset,scale} => 
	seq [layoutAtom base,
	     str "[",
	     case scale
	      of 1 =>
		 str (Int.fmt StringCvt.DEC offset)
	       | _ =>
		 align [str (Int.fmt StringCvt.DEC scale),
			str "*",
			str (Int.fmt StringCvt.DEC offset)],
	     str "]"]

and layoutLiteral lit
  = case lit
     of Spil.Real r => str (Real.fmt StringCvt.EXACT r)
      | Spil.Int z => str (Int.fmt StringCvt.DEC z)

and layoutTransfer (b as Spil.Block.BLOCK {transfer, ...})
  = case transfer
     of Spil.Control.CALL {func, args, return}
	=> stm (seq [str func, str "()"])

      | Spil.Control.JUMP {block, args}
	=> stm (seq [str block, str "()"])

      | Spil.Control.SWITCH {test, cases, default}
	=> align [str "switch (0) {",
		  str "}"]

      | Spil.Control.RETURN value
	=> stm (str "return")

and mainDefinition (Spil.Program.PROGRAM {main,...})
  = align [comment ["= Main ="],
	   layoutFunctionDefinition main]


    
end
