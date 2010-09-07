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

fun vec f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i)))
fun veci f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i),i))

fun sub t = indent (t,4)
fun subs t = indent (align t,4)

fun size s = str (Int.toString s)
fun size2d {x,y} = seq [size x, str ",", size y]
fun size3d {x,y,z} = seq [size x, str ",", size y, str ",", size z]

end
open Layout



fun spilToScalar (program)
  = align [
    comment ["Copyright (c) 2010 by Simatra Modeling Technologies, L.L.C"],
    typeDeclarations program,
    globalDefinitions program,
    functionPrototypes program,
    functionDefinitions program,
    mainDefinition program,
    comment ["End of program"]
    ]

and typeDeclarations (program)
  = align [
    comment ["= Type Declarations ="]
    ]

and globalDefinitions (program)
  = comment ["= Global Variable Definitions ="]

and functionPrototypes (program)
  = align [
    comment ["= Function Prototypes ="],
    align (Spil.Program.foldFunctions
	       (fn (f,lyt) => (stm (layoutFunctionPrototype f))::lyt) 
	       nil program)
    ]

and layoutFunctionPrototype (f as Spil.Function.FUNCTION {name, returns, ...}) 
  = space [layoutType returns, str name, layoutFunctionParams f]

and layoutNormalType trep
  = case trep
     of Type.Int bits => seq [str "int", str (Int.toString (Size.Bits.toInt bits))]
      | Type.Real bits => seq [str "real", str (Int.toString (Size.Bits.toInt bits))]
      | Type.String => str "char *"
      | Type.Bool => str "bool"
      | Type.Var var => seq [str (Type.varName var), str "_t"]
      | Type.Arrow (t1,t2) => seq [layoutNormalType t1, str "(*FIXME_function)(", layoutNormalType t2, str ")"]
      | Type.Record fs
	=> seq [str "struct {",
		seq (vec (fn (f,s) => stm (space [str f, layoutNormalType s])) fs),
		str "}"]
      | Type.Array => seq [str "(void *)"]
      | Type.Abstract (var,t2) => seq [str (Type.varName var), layoutNormalType t2]
      | Type.Apply (Array,t2) => seq [str "(", layoutNormalType t2, str " *)"]

and layoutType t = layoutNormalType (Type.rep (Type.normal t))

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
  = align [
    comment ["Function " ^ name],
    blockPrototypes f,
    blockDefinitions f,
    space [layoutFunctionPrototype f, str "{"],
    sub (layoutFunctionBody f),
    str "}"
    ]

and layoutFunctionBody (f as Spil.Function.FUNCTION {start, ...})
  = align[
    stm (space [str "goto", str start])
    ]
    
and layoutBlockPrototype (b)
  = space [str "void", 
	   str (Spil.Block.name b), 
	   layoutBlockParams b]

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
  = align [
    space [layoutBlockPrototype b, str "{"],
    subs [align(Spil.Block.foldBody
		    (fn (s,lyt) => (layoutStatement s)::lyt)
		    nil b),
	  layoutTransfer b],
    str "}"]


and layoutStatement (statement)
  = case statement
     of Spil.Statement.HALT 
	=> stm (str "exit(1)")

      | Spil.Statement.NOP
	=> stm (str "")

      | Spil.Statement.COMMENT text
	=> comment [text]

      | Spil.Statement.BIND {src, dest}
	=> bindExpression (dest, Spil.Expression.Value src)

      | Spil.Statement.GRAPH {src, dest}
	=> bindExpression (dest, src)

      | Spil.Statement.PRIMITIVE {oper, args, dest}
	=> bindExpression (dest, Spil.Expression.Apply {oper= oper, args= Vector.map Spil.Expression.Value args})

      | Spil.Statement.MOVE {src, dest} =>
	stm (space [layoutAtom dest, str "=", layoutAtom src])

      | Spil.Statement.PROFILE _ 
	=> stm (str "PROFILE")

and bindExpression ((id,t), src) =
    stm (space [layoutType t, str id, str "=", layoutExpression src])

and layoutExpression (exp)
  = case exp
     of Spil.Expression.Value atom => layoutAtom atom
      | Spil.Expression.Apply {oper, args}
	=> seq [str (Spil.Operator.name oper),
		tuple (vec layoutExpression args)]

and layoutAtom atom
  = case atom
     of Spil.Atom.Null => str "0"
      | Spil.Atom.Variable id => str id
      | Spil.Atom.RuntimeVar id => seq [str "Var_FIXME"]
      | Spil.Atom.CompileVar id => seq [str "Const_FIXME"]
      | Spil.Atom.Address addr => str addr
      | Spil.Atom.Label id => str id
      | Spil.Atom.Literal lit => layoutLiteral lit
      | Spil.Atom.Cast (base, basetype) 
	=> seq [str "((", layoutType basetype, str ")", layoutAtom base, str ")"]
      | Spil.Atom.Offset {base, index, offset, scale, basetype}
	=> seq [str "OFFSET",
		parenList [layoutAtom base,
			   size index,
			   size offset,
			   size scale,
			   layoutType basetype]]
      | Spil.Atom.Offset2D {base, index, offset, scale, basetype}
	=> seq [str "OFFSET2D",
		parenList [layoutAtom base,
			   size2d index,
			   size2d offset,
			   size2d scale,
			   layoutType basetype]]
      | Spil.Atom.Offset3D {base, index, offset, scale, basetype}
	=> seq [str "OFFSET3D",
		parenList [layoutAtom base,
			   size3d index,
			   size3d offset,
			   size3d scale,
			   layoutType basetype]]

and layoutLiteral lit
  = case lit
     of Spil.Real r => str (Real.fmt StringCvt.EXACT r)
      | Spil.Int z => str (Int.fmt StringCvt.DEC z)
      | Spil.Bool b => str (if b then "YES" else "NO")

and layoutTransfer (b as Spil.Block.BLOCK {transfer, ...})
  = case transfer
     of Spil.Control.CALL {func, args, return}
	=> stm (seq [str "return ", str func, str "()"])

      | Spil.Control.JUMP {block, args}
	=> stm (space [str "return", seq [str block, str "()"]])

      | Spil.Control.SWITCH {test, cases, default}
	=> align [str "switch (0) {",
		  seq [space [str "default:", str "return", seq [str default, str "()"]]],
		  str "}"]

      | Spil.Control.RETURN value
	=> stm (space [str "return", layoutAtom value])

and mainDefinition (Spil.Program.PROGRAM {main,...})
  = align [comment ["= Main ="],
	   layoutFunctionDefinition main]


    
end
