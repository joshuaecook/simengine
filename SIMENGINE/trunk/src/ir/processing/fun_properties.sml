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

signature FUNPROPS =
sig

(* Duplicate the data types *)
type fix
type operands

(* return information about operations *)
val op2name : Fun.funtype -> string (* return the name of an operation as a string *)
val fun2textstrnotation : Fun.funtype -> (string * fix) (* accessor to determine how to display op as text *)
val fun2cstrnotation : Fun.funtype -> (string * fix) (* accessor to determine how to display op as C code *)
val fun2mathematicastrnotation : Fun.funtype -> (string * fix) (* accessor to determine how to display op as Mathematica code *)
val hasVariableArguments : Fun.funtype -> bool (* operations like ADD and MUL can allow arbitrary numbers of operands *)

end

structure FunProps : FUNPROPS =
struct

type fix = MathFunctionProperties.fix
type operands = MathFunctionProperties.operands
val PREFIX = MathFunctionProperties.PREFIX

open Fun

fun op2name (f: funtype) = 
    case f
     of BUILTIN v => MathFunctionProperties.op2name v
      | INST {classname,...} => Symbol.name classname
      | OUTPUT {outname, ...} => Symbol.name outname

fun fun2textstrnotation f =
    case f 
     of BUILTIN v => MathFunctionProperties.fun2textstrnotation v
      | INST {classname,instname,props,...} => 
	(case InstProps.getRealClassName props of
	     SOME class_sym =>
	     if classname = class_sym then
		 ((Symbol.name instname) ^ "<"^(Symbol.name classname)^">", PREFIX)
	     else
		 ((Symbol.name instname) ^ "<"^(Symbol.name classname)^":"^(Symbol.name class_sym)^">", PREFIX)
	   | _ => 
	     ((Symbol.name instname) ^ "<"^(Symbol.name classname)^">", PREFIX))
      | OUTPUT {classname,instname,outname,props,...} =>
	(case InstProps.getRealClassName props of
	     SOME class_sym =>
	     if classname = class_sym then
		 ((Symbol.name instname) ^ "<"^(Symbol.name classname)^">."^(Symbol.name outname), PREFIX)
	     else
		 ((Symbol.name instname) ^ "<"^(Symbol.name classname)^":"^(Symbol.name class_sym)^">."^(Symbol.name outname), PREFIX)
	   | _ => 
	     ((Symbol.name instname) ^ "<"^(Symbol.name classname)^">."^(Symbol.name outname), PREFIX))
	


fun fun2cstrnotation f =
    case f 
     of BUILTIN v => MathFunctionProperties.fun2cstrnotation v
      | INST {classname,...} => (Symbol.name classname, PREFIX)
      | OUTPUT {outname, ...} => (Symbol.name outname, PREFIX)

fun fun2mathematicastrnotation f =
    case f 
     of BUILTIN v => MathFunctionProperties.fun2mathematicastrnotation v
      | INST {classname,...} => (Symbol.name classname, PREFIX)
      | OUTPUT {outname, ...} => (Symbol.name outname, PREFIX)

fun hasVariableArguments f =
    case f 
     of BUILTIN v => MathFunctionProperties.hasVariableArguments v
      | INST _ => false (* not allowing variable arguments for instances *)
      | OUTPUT _ => false






end
