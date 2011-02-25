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

structure ComparisonLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments
and reals2boolfun = LibraryUtil.reals2boolfun

fun error msg =
    Logger.log_error (Printer.$ msg)

fun gt _ = reals2boolfun Real.>
fun ge _ = reals2boolfun Real.>=
fun lt _ = reals2boolfun Real.<
fun le _ = reals2boolfun Real.<=

fun eq _ args =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r1), KEC.LITERAL(KEC.CONSTREAL r2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (Real.?=(r1, r2) andalso ((Real.isNan r1) = (Real.isNan r2))))
      |	[KEC.LITERAL(KEC.CONSTSTR s1), KEC.LITERAL(KEC.CONSTSTR s2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (s1 = s2)) 
      |	[KEC.LITERAL(KEC.CONSTBINARY (size1, val1)), KEC.LITERAL(KEC.CONSTBINARY (size2, val2))] =>
	KEC.LITERAL(KEC.CONSTBOOL (size1 = size2 andalso val1 = val2))
     |	[KEC.LITERAL(KEC.CONSTBOOL b1), KEC.LITERAL(KEC.CONSTBOOL b2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (b1 = b2))
      | [KEC.OBJECT {members=members1, ...}, KEC.OBJECT {members=members2, ...}]
	=> KEC.LITERAL(KEC.CONSTBOOL(members1 = members2))
      | [KEC.UNIT, KEC.UNIT]
	=> KEC.LITERAL(KEC.CONSTBOOL(true))
      | [arg1, arg2]
	=> KEC.LITERAL(KEC.CONSTBOOL(false))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

fun ne _ args =
    case args of
	[KEC.LITERAL(KEC.CONSTREAL r1), KEC.LITERAL(KEC.CONSTREAL r2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (not (Real.?=(r1, r2))))
      |	[KEC.LITERAL(KEC.CONSTSTR s1), KEC.LITERAL(KEC.CONSTSTR s2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (s1 <> s2))
      |	[KEC.LITERAL(KEC.CONSTBINARY (size1, val1)), KEC.LITERAL(KEC.CONSTBINARY (size2, val2))] =>
	KEC.LITERAL(KEC.CONSTBOOL (size1 <> size2 orelse val1 <> val2))
      |	[KEC.LITERAL(KEC.CONSTBOOL b1), KEC.LITERAL(KEC.CONSTBOOL b2)] =>
	KEC.LITERAL(KEC.CONSTBOOL (b1 <> b2))
      | [KEC.UNIT, KEC.UNIT]
	=> KEC.LITERAL(KEC.CONSTBOOL(false))
      | [KEC.OBJECT {members=members1, ...}, KEC.OBJECT {members=members2, ...}]
	=> KEC.LITERAL(KEC.CONSTBOOL(members1 <> members2))
      | [arg1, arg2]
	=> KEC.LITERAL(KEC.CONSTBOOL(true))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}
	
fun isdefined _ args =
    case args of
	[KEC.UNDEFINED] => KEC.LITERAL (KEC.CONSTBOOL false)
      | [_] => KEC.LITERAL (KEC.CONSTBOOL true)
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="gt", operation=gt},
	       {name="ge", operation=ge},
	       {name="lt", operation=lt},
	       {name="le", operation=le},
	       {name="eq", operation=eq},
	       {name="neq", operation=ne},
	       {name="isdefined", operation=isdefined}
	      ]

end
