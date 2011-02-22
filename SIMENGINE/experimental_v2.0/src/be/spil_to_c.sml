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
fun declare (id,t) = stmt (space [t, str id])
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
fun veci f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i), i))

fun layoutFunction (f as F.Function function) =
    let
	fun layoutParam (id, t) =
	    L.space [layoutType t, L.str id]

	fun layoutLocal id =
	    L.declare (id, layoutType (T.C"CDATAFORMAT"))

	fun layoutBlockParams (b as B.Block block) =
	    if Vector.length (#params block) > 0 then
		L.declare ((B.name b)^"_args", 
			   L.align [L.str "struct {",
				    L.sub (vec (fn (id,t) => L.declare (id, layoutType t)) (#params block)),
				    L.str "}"])
	    else L.empty

	(* Find the start block and lay it out first. *)
	val firstBlockId =
	    case Vector.findi (fn (i, B.Block block) => (#start function) = (#label block)) (#blocks function)
	     of SOME (i,_) => i
	      | NONE => DynException.stdException(("Malformed function: no block named "^(#start function)), "SpilToC.layoutFunction", Logger.INTERNAL)
    in
	L.align
	    [L.space [layoutType (#returns function),
		    L.str (#name function),
		    L.tuple (vec layoutParam (#params function))],
	     L.str "{",
	     L.comment "Function local declarations",
	     L.sub (List.map layoutLocal (F.Locals.listItems (F.locals f))),
	     L.comment "Block parameter declarations",
	     if Vector.exists (fn (B.Block b) => Vector.length (#params b) > 0) (#blocks function) then
		 L.sub [
		 L.declare 
		     ("block_args",
		      L.align
			  [L.str "union {",
			   L.sub (vec layoutBlockParams (#blocks function)),
			   L.str "}"])]
	     else L.empty,
	     L.goto (B.name (Vector.sub (#blocks function, firstBlockId))),
	     L.comment "Block definitions",
	     layoutBlock f (Vector.sub (#blocks function, firstBlockId)),
	     L.align (veci (fn (b,i) => if i = firstBlockId then L.empty else layoutBlock f b) (#blocks function)),
	     L.str "}"
	    ]
    end

and layoutBlock function (b as B.Block block) =
    let
	fun parameter (id,t) =
	    L.assign (L.space [layoutType t, L.str id], 
		      L.seq [L.str "block_args.", L.str (B.name b), L.str "_args.", L.str id]) 
    in
	L.align
	    [L.seq [L.str (B.name b), L.str ":"],
	     L.comment ("free: " ^ (String.concatWith "," (B.Free.listItems (B.free b)))),
	     L.sub
		 [L.str "{", 
		  L.align (vec parameter (#params block)),
		  L.align (vec layoutStatement (#body block)),
		  layoutControl function (#transfer block),
		  L.str "}"]
	    ]
    end

and layoutStatement statement =
    case statement
     of S.Halt => L.exit
      | S.Nop => L.stmt (L.empty)
      | S.Comment str => L.comment str
      | S.Profile id => L.profile id
      | S.Bind {src, dest as (id,t)} =>
	L.assign (L.space [layoutType t, L.str id], layoutAtom src) 
      | S.Graph {src, dest as (id,t)} =>
	L.assign (L.space [layoutType t, L.str id], layoutExpression false src) 
      | S.Primitive {oper, args, dest as (id,t)} =>
	L.assign (L.space [layoutType t, L.str id], layoutOperator false (oper, Vector.map X.Value args))
      | S.Move {src, dest} =>
	L.assign (layoutAtom dest, layoutAtom src)

and layoutControl function control =
    case control
     of CC.Return value => 
	L.return (layoutAtom value)
      | CC.Switch {test, cases, default} => 
	let
	    fun layoutCase (match, cc) =
		L.align
		    [L.space [L.str "case", layoutImmediate match, L.str ":"],
		     L.sub
			 [layoutControl function cc,
			  L.stmt (L.str "break")]
		    ]
	in
	    case Vector.length cases
	     of 0 => layoutControl function default
	      | 1 =>
		let val (match, cc) = Vector.sub (cases, 0) in
		    L.align
			[L.space [L.str "if", L.paren (L.equals (layoutAtom test, layoutImmediate match))],
			 layoutControl function cc,
			 L.str "else",
			 layoutControl function default
			]
		end
	      | _ =>
		L.align
		    [L.space [L.str "switch", L.paren (layoutAtom test)],
		     L.str "{",
		     L.align (vec layoutCase cases),
		     L.str "default :",
		     layoutControl function default,
		     L.str "}"
		    ]
	end
      | CC.Jump {block, args} =>
	let
	    val params = 
		case F.findBlock (fn b => (B.name b) = block) function
		 of SOME (B.Block {params, ...}) => params
		  | NONE => DynException.stdException(("Non-local jump to block named "^block), "SpilToC.layoutControl", Logger.INTERNAL)
			    
	    fun parameter idx = #1 (Vector.sub (params, idx))
	    fun argument (value, idx) =
		L.assign (L.seq [L.str "block_args.", L.str block, L.str "_args.", L.str (parameter idx)], 
			  layoutAtom value)
	in
	    if Vector.length args > 0 then
		L.align
		    [L.align (veci argument args),
		     L.goto block]
	    else L.goto block
	end
      | CC.Call {func, args, return} =>
	let val call = L.call (func, vec layoutAtom args) in
	    case return
	     of NONE => L.return call
	      | SOME cc => L.align [L.stmt call, layoutControl function cc]
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
	      | Op.Float_neg =>
		(case Vector.length args
		  of 1 => [L.str "-", layoutExpression true (Vector.sub (args,0))]
		   | _ => [L.unimplemented "Float_neg"])
	      | Op.Array_extract =>
		(case Vector.length args
		  of 2 => [layoutExpression true (Vector.sub (args,0)), L.bracket (layoutExpression false (Vector.sub (args,1)))]
		   | _ => [L.unimplemented "Array_extract"])
	      | Op.Record_extract =>
		(case Vector.length args
		  of 2 => [layoutExpression true (Vector.sub (args,0)), L.str ".", layoutExpression true (Vector.sub (args,1))]
		   | _ => [L.unimplemented "Record_extract"])
	      | Op.Address_addr =>
		(case Vector.length args
		  of 1 => [L.str "&", layoutExpression true (Vector.sub (args,0))]
		   | _ => [L.unimplemented "Address_addr"])
	      | Op.Address_deref =>
		(case Vector.length args
		  of 1 => [L.str "*", layoutExpression true (Vector.sub (args,0))]
		   | _ => [L.unimplemented "Address_deref"])
	      | Op.Sim_if =>
		(case Vector.length args
		  of 3 => [layoutExpression true (Vector.sub (args,0)), L.str "?", 
			   layoutExpression true (Vector.sub (args,1)), L.str ":", 
			   layoutExpression true (Vector.sub (args,2))]
		   | _ => [L.unimplemented "Cell_ref"])
	      | _ => 
		let 
		    fun rename name =
			if String.isPrefix "Math_" name then
			    String.extract (name, String.size "Math_", NONE)
			else name
		    val opname = rename (Op.name oper)
		in
		    [L.call (opname, vec (layoutExpression false) args)]
		end
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
      | _ => L.unimplemented "layoutAtom"

and layoutImmediate literal =
    case literal
     of Real r => L.str r
      | Int z => L.str z
      | Bool b => (case b of "no" => L.str "0" | _ => L.str "1")
      | String s => L.string s
      | Const id => L.str id
      | Nan => L.str "NAN"
      | Infinity => L.str "INFINITY"

and layoutType t =
    case T.rep t
     of T.CType name => L.str name
      | _ => L.unimplemented "layoutType"



end
