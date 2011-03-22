(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

structure SpilToC: sig
    val layoutFunction: Spil.Function.t -> Layout.t
end = struct
exception Unimplemented of string

open Spil
structure T = Type
structure F = Function
structure B = Block
structure C = Control
structure A = Atom

structure L = struct
open Layout
fun stmt lay = seq [lay, str ";"]
fun equals (left, right) = space [left, str "==", right]
fun return lay = stmt (space [str "return", lay])
fun goto label = stmt (space [str "goto", str label])
val real = str o (Real.fmt StringCvt.EXACT)
val int = str o (Int.fmt StringCvt.DEC)
val bool = str o (fn true => "YES" | false => "NO")
end

fun vec f v = List.tabulate (Vector.length v, fn i => f (Vector.sub (v,i)))
fun veci f v = List.tabulate (Vector.length v, fn i => f (i,Vector.sub (v,i)))

fun layoutFunction (F.FUNCTION function) =
    let
	fun layoutParam (id, t) =
	    L.space [layoutType t, L.str id]

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
	     layoutBlock (Vector.sub (#blocks function, firstBlockId)),
	     L.align (veci (fn (i,b) => if i = firstBlockId then L.empty else layoutBlock b) (#blocks function)),
	     L.str "}"
	    ]
    end

and layoutBlock (B.BLOCK block) =
    let
    in
	L.align
	    [L.seq [L.str (#label block), L.str ":"],
	     L.indent (L.str "{", 2),
	     L.indent (L.align (vec layoutStatement (#body block)), 2),
	     L.indent (layoutControl (#transfer block), 2),
	     L.indent (L.str "}", 2)
	    ]
    end

and layoutStatement statement =
    let
    in
	raise Unimplemented "layoutStatement"
    end

and layoutControl control =
    case control
     of C.RETURN value => 
	L.return (layoutAtom value)
      | C.SWITCH {test, cases, default} => 
	let
	    fun layoutCase (value, label) =
		L.align
		    [L.space [L.str "case", layoutImmediate value, L.str ":"],
		     L.indent (L.goto label, 2),
		     L.indent (L.stmt (L.str "break"), 2)
		    ]
	in
	    case Vector.length cases
	     of 0 => L.indent (L.goto default, 2)
	      | 1 =>
		let val (value, label) = Vector.sub (cases, 0) in
		    L.align
			[L.space [L.str "if", L.paren (L.equals (layoutAtom test, layoutImmediate value))],
			 L.indent (L.goto label, 2),
			 L.str "else",
			 L.indent (L.goto default, 2)
			]
		end
	      | _ =>
		L.align
		    [L.space [L.str "switch", L.paren (layoutAtom test)],
		     L.str "{",
		     L.align (vec layoutCase cases),
		     L.str "default :",
		     L.indent (L.goto default, 2),
		     L.str "}"
		    ]
	end
      | C.JUMP {block, args} =>
	raise Unimplemented "layoutControl"
      | C.CALL {func, args, return} =>
	case return
	 of NONE => L.return (L.seq [L.str func, L.tuple (vec layoutAtom args)])
	  | SOME _ => raise Unimplemented "layoutControl"

and layoutAtom atom =
    case atom
     of A.Null => L.str "NULL"
      | A.Variable id => L.str id
      | A.Literal lit => layoutImmediate lit
      | A.Cast (atom, t) => L.seq [L.paren (layoutType t), layoutAtom atom]
      | _ => raise Unimplemented "layoutAtom"

and layoutImmediate literal =
    case literal
     of Real r => L.real r
      | Int z => L.int z
      | Bool b => L.bool b
      | Const id => L.str id

and layoutType t =
    case T.rep t
     of T.CType name => L.str name
      | _ => raise Unimplemented "layoutType"



end
