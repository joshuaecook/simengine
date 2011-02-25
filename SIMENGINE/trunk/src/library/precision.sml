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

structure PrecisionLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

exception Aborted

fun std_quantify _ (args: KEC.exp list) =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r)] => KEC.LITERAL(KEC.CONSTREAL (RangeBits.real2quantreal r))
      | [exp] => raise TypeMismatch ("expected a number but received " ^ (PrettyPrint.kecexp2nickname exp))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_num2bits exec (args: KEC.exp list) =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r)] => 
	let
	    val (_, {sign, bits, frac}) = RangeBits.num2bits r (RangeBits.accuracy())

	in
	    exec (KEC.APPLY{func=KEC.SEND {message=Symbol.symbol "new",
					   object=KEC.SYMBOL (Symbol.symbol "FixptPrecision")}, 
			    args=KEC.TUPLE [KEC.LITERAL(KEC.CONSTREAL (Real.fromInt sign)),
					    KEC.LITERAL(KEC.CONSTREAL (Real.fromInt bits)),
					    KEC.LITERAL(KEC.CONSTREAL (Real.fromInt frac))]})
	end		 
      | [exp] => raise TypeMismatch ("expected a number but received " ^ (PrettyPrint.kecexp2nickname exp))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="quantify", operation=std_quantify},
	       {name="num2fixpt", operation=std_num2bits}]

end
