signature PRINT_SPIL = sig

structure Syntax: SPIL

val layout: Syntax.fragment -> Layout.t
end


functor MakeSpilPrinter (structure Syntax: SPIL) = struct

structure Syntax = Syntax
open Syntax

structure Layout = struct
open Layout

val comment =
 fn nil => empty
  | [line] => seq [str "'''", str line, str "'''"]
  | line::more => align [seq [str "'''", str line], align (map str more), str "'''"]

fun stm line = seq [line, str ";"]
fun vec f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i)))
fun veci f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i),i))

fun sub t = indent (t,2)
fun subs t = sub (align t)

end
open Layout

exception Unimplemented

fun layout (ATOM atom) = layoutAtom atom
  | layout (STATEMENT stm) = layoutStatement stm
  | layout (CONTROL control) = layoutControl control
  | layout (BLOCK block) = layoutBlock block
  | layout (FUNCTION function) = layoutFunction function
  | layout (PROGRAM program) = layoutProgram program
  | layout (TYPENAME typename) = layoutType typename
  | layout (OPERATOR oper) = str (Operator.name oper)
  | layout _ = raise Unimplemented

and layoutAtom atom =
    case atom
     of Atom.Variable id => str id
      | Atom.Literal lit => layoutLiteral lit
      | _ => raise Unimplemented

and layoutLiteral lit
  = case lit
     of Real r => str r
      | Int z => str z
      | Bool b => str b
      | Const id => str id
      | String text => seq [str "'", str text, str "'"]
      | Infinity => str "Infinity"
      | Nan => str "Nan"
      | Undefined => str "Undefined"

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
      | _ => raise Unimplemented

and layoutType t = 
    case Type.rep (Type.normal t)
     of Type.CType id => seq [str "C\"", str id, str "\""]
      | trep => seq [str "\"", layoutNormalType trep, str "\""]

and layoutStatement (statement) =
    case statement
     of Statement.Halt 
	=> stm (str "halt")

      | Statement.Nop
	=> stm (empty)

      | Statement.Comment text
	=> comment [text]

      | Statement.Bind {src, dest as (id,t)}
	=> stm (space [seq [str id, str ":"], layoutType t, str "=", layoutAtom src])

      | Statement.Graph {src, dest as (id,t)}
	=> stm (space [seq [str id, str ":"], layoutType t, str "=", layoutExpression src])

      | Statement.Primitive {oper, args, dest as (id,t)}
	=> stm (space [seq [str id, str ":"], layoutType t, str "=", str (Operator.name oper), layoutArgs args])

      | Statement.Move {src, dest} =>
	stm (space [layoutAtom dest, str "=", layoutAtom src])

      | _ => raise Unimplemented

and layoutExpression (exp) =
    case exp
     of Expression.Value atom => layoutAtom atom
      | Expression.Apply {oper, args}
	=> space [str (Operator.name oper), tuple (vec layoutExpression args)]

and layoutControl control =
    case control
     of Control.Jump {block, args}
	=> stm (space [str "jump", str block, layoutArgs args])

      | Control.Switch {test, cases, default}
	=> align [
	   space [str "switch", seq [str "(", layoutAtom test, str ")"], str "{"],
	   subs (vec layoutSwitchcase cases),
	   space [str "}", str "default", layoutControl default]]

      | Control.Return atom
	=> space [str "return", layoutAtom atom]

      | _ => raise Unimplemented

and layoutSwitchcase (match, transfer) =
    stm (space [layoutLiteral match, str "=>", layoutControl transfer])

and layoutArgs args =
    space (vec layoutAtom args)

and layoutParams args =
    parenList (vec layoutParam args)

and layoutParam (id,t) =
    space [seq [str id, str ":"], layoutType t]

and layoutBlock (Block.Block {label, params, body, transfer}) =
    align [
    space [seq [str label, str ":"], layoutParams params],
    subs (vec layoutStatement body),
    sub (layoutControl transfer)]

and layoutFunction (Function.Function {name, params, start, blocks, returns}) =
    align [
    space [str name, layoutParams params, str ":", layoutType returns, str "{"],
    subs (vec layoutBlock blocks),
    str "}"]

and layoutProgram (Program.Program {types, globals, functions, main}) =
    align [
    space [str "program", str "{"],
    subs (vec layoutFunction functions),
    str "}",
    layoutFunction main]

end
