structure SpilToC: sig
    val layoutFunction: Spil.Function.t -> Layout.t
end = struct

open Spil
structure T = Type
structure F = Function
structure B = Block
structure S = Statement
structure CC = Control
structure A = Atom
structure Op = Operator
structure X = Expression

structure L = struct
open Layout
fun stmt lay = seq [lay, str ";"]
fun sub text = indent (align text, 2)
fun equals (left, right) = space [left, str "==", right]
fun assign (left, right) = stmt (space [left, str "=", right])
fun return lay = stmt (space [str "return", lay])
fun goto label = stmt (space [str "goto", str label])
local val tr = String.map (fn #"~" => #"-" | c => c) in
fun real r = str (tr (Real.fmt StringCvt.EXACT r))
fun int z = str (tr (Int.fmt StringCvt.DEC z))
end
val bool = str o (fn true => "YES" | false => "NO")
fun string s = seq (map str ["\"", String.toCString s, "\""])
fun comment s = if ! detailed then seq (map str ["// ", s]) else empty
fun call (oper, args) = seq [str oper, tuple args]
val exit = stmt (call ("exit", [int ~1]))
fun profile id = call ("PROFILE", [string id])
fun unimplemented id = call ("#error",[str "unimplemented", string id])
end

fun vec f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i)))
fun veci f v = List.tabulate (Vector.length v, fn i => f (i,Vector.sub (v,i)))

fun layoutFunction (f as F.FUNCTION function) =
    let
	fun layoutLocal id =
	    L.stmt (L.space [layoutType (T.C"CDATAFORMAT"), L.str id])

	fun layoutParam (id, t) =
	    L.space [layoutType t, L.str id]

	(* Determine which variables are local by subtracting the
	 * function's parameters from the union of all blocks' free variables. *)
	val locals =
	    let
		val free =
		    F.foldBlocks
			(fn (b, set) => B.Free.union (set, B.free b))
			B.Free.empty f
	    in
		B.Free.difference 
		    (free,
		     F.foldParams
			 (fn ((id,_), set) => B.Free.add (set, id))
			 B.Free.empty f
		    )
	    end

	(* Find the start block and lay it out first. *)
	val firstBlockId =
	    case Vector.findi (fn (i, B.BLOCK block) => (#start function) = (#label block)) (#blocks function)
	     of SOME (i,_) => i
	      | NONE => DynException.stdException(("Malformed function: no block named "^(#start function)), "SpilToC.layoutFunction", Logger.INTERNAL)
    in
	L.align
	    [L.space [layoutType (#returns function),
		    L.str (#name function),
		    L.tuple (vec layoutParam (#params function))],
	     L.str "{",
	     L.sub (List.map layoutLocal (B.Free.listItems locals)),
	     layoutBlock (Vector.sub (#blocks function, firstBlockId)),
	     L.align (veci (fn (i,b) => if i = firstBlockId then L.empty else layoutBlock b) (#blocks function)),
	     L.str "}"
	    ]
    end

and layoutBlock (b as B.BLOCK block) =
    let
	val free = B.free b
    in
	L.align
	    [L.seq [L.str (#label block), L.str ":"],
	     L.comment ("free: " ^ (String.concatWith "," (B.Free.listItems free))),
	     L.sub
		 [L.str "{", 
		  L.align (vec layoutStatement (#body block)),
		  layoutControl (#transfer block),
		  L.str "}"]
	    ]
    end

and layoutStatement statement =
    case statement
     of S.HALT => L.exit
      | S.NOP => L.stmt (L.empty)
      | S.COMMENT str => L.comment str
      | S.PROFILE id => L.profile id
      | S.BIND {src, dest as (id,t)} =>
	L.assign (L.space [layoutType t, L.str id], layoutAtom src) 
      | S.GRAPH {src, dest as (id,t)} =>
	L.assign (L.space [layoutType t, L.str id], layoutExpression false src) 
      | S.PRIMITIVE {oper, args, dest as (id,t)} =>
	L.assign (L.space [layoutType t, L.str id], layoutOperator false (oper, Vector.map X.Value args))
      | S.MOVE {src, dest} =>
	L.assign (layoutAtom dest, layoutAtom src)

and layoutControl control =
    case control
     of CC.RETURN value => 
	L.return (layoutAtom value)
      | CC.SWITCH {test, cases, default} => 
	let
	    fun layoutCase (value, label) =
		L.align
		    [L.space [L.str "case", layoutImmediate value, L.str ":"],
		     L.sub
			 [L.goto label,
			  L.stmt (L.str "break")]
		    ]
	in
	    case Vector.length cases
	     of 0 => L.sub [L.goto default]
	      | 1 =>
		let val (value, label) = Vector.sub (cases, 0) in
		    L.align
			[L.space [L.str "if", L.paren (L.equals (layoutAtom test, layoutImmediate value))],
			 L.sub [L.goto label],
			 L.str "else",
			 L.sub [L.goto default]
			]
		end
	      | _ =>
		L.align
		    [L.space [L.str "switch", L.paren (layoutAtom test)],
		     L.str "{",
		     L.align (vec layoutCase cases),
		     L.str "default :",
		     L.sub [L.goto default],
		     L.str "}"
		    ]
	end
      | CC.JUMP {block, args} =>
	L.goto block
      | CC.CALL {func, args, return} =>
	let val call = L.call (func, vec layoutAtom args) in
	    case return
	     of NONE => L.return call
	      | SOME cc => L.align [L.stmt call, layoutControl cc]
	end

and layoutExpression paren expr =
    case expr
     of X.Value atom => layoutAtom atom
      | X.Apply {oper, args} => layoutOperator paren (oper, args)

and layoutOperator paren (oper, args) = 
    let 
	val lay = 
	    case oper
	     of Op.Float_add =>
		L.separate (vec (layoutExpression true) args, " + ")
	      | Op.Float_sub =>
		L.separate (vec (layoutExpression true) args, " - ")
	      | Op.Float_mul =>
		L.separate (vec (layoutExpression true) args, " * ")
	      | Op.Float_div =>
		L.separate (vec (layoutExpression true) args, " / ")
	      | Op.Float_gt =>
		L.separate (vec (layoutExpression true) args, " > ")
	      | Op.Array_extract =>
		(case Vector.length args
		  of 2 => [layoutExpression true (Vector.sub (args,0)), L.bracket (layoutExpression false (Vector.sub (args,1)))]
		   | _ => [L.unimplemented "Array_extract"])
	      | Op.Record_extract =>
		(case Vector.length args
		  of 2 => [layoutExpression true (Vector.sub (args,0)), L.str ".", layoutExpression true (Vector.sub (args,1))]
		   | _ => [L.unimplemented "Record_extract"])
	      | Op.Cell_ref =>
		(case Vector.length args
		  of 1 => [L.str "&", layoutExpression true (Vector.sub (args,0))]
		   | _ => [L.unimplemented "Cell_ref"])
	      | Op.Cell_deref =>
		(case Vector.length args
		  of 1 => [L.str "*", layoutExpression true (Vector.sub (args,0))]
		   | _ => [L.unimplemented "Cell_ref"])
	      | _ => 
		[L.call (Op.name oper, vec (layoutExpression false) args)]
    in
	if paren then L.paren (L.seq lay) else L.seq lay
    end

and layoutAtom atom =
    case atom
     of A.Null => L.str "NULL"
      | A.Variable id => L.str id
      | A.Symbol id => L.str id
      | A.Literal lit => layoutImmediate lit
      | A.CompileVar (f, t) => layoutAtom (f ())
      | A.RuntimeVar (f, t) => layoutAtom (f ())
      | A.Source atom => L.seq [L.str "&", layoutAtom atom]
      | A.Sink atom => L.seq [L.str "*", layoutAtom atom]
      | A.Cast (atom, t) => L.seq [L.paren (layoutType t), layoutAtom atom]
      | A.Offset {base, index, offset, scale, basetype} =>
	L.paren (L.seq [L.paren (layoutType basetype), layoutAtom base, L.str"[", L.int ((index*scale)+offset), L.str"]"])
	(* L.call ("OFFSET", [layoutAtom base, L.int index, L.int offset, L.int scale, layoutType basetype]) *)
      | A.Offset2D {base, index, offset, scale, basetype} =>
	L.call ("OFFSET2D", [layoutAtom base, 
			     L.int (#x index), L.int (#y index), 
			     L.int (#x offset), L.int (#y offset), 
			     L.int (#x scale), L.int (#y scale), 
			     layoutType basetype])
      | A.Offset3D {base, index, offset, scale, basetype} =>
	L.call ("OFFSET3D", [layoutAtom base, 
			     L.int (#x index), L.int (#y index), L.int (#z index), 
			     L.int (#x offset), L.int (#y offset), L.int (#z offset), 
			     L.int (#x scale), L.int (#y scale), L.int (#z scale),
			     layoutType basetype])
      | _ => L.unimplemented "layoutAtom"

and layoutImmediate literal =
    case literal
     of Real r => L.real r
      | Int z => L.int z
      | Bool b => L.bool b
      | String s => L.string s
      | Const id => L.str id
      | Nan => L.str "NAN"
      | Infinity => L.str "INFINITY"

and layoutType t =
    case T.rep t
     of T.CType name => L.str name
      | _ => L.unimplemented "layoutType"



end
