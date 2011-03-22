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

structure TimeLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun sys_timestampSeconds _ _ =
    KEC.LITERAL (KEC.CONSTREAL (Real.fromLargeInt(Time.toSeconds(Time.now()))))
    handle e => DynException.checkpoint "SystemLib.timestampSeconds" e

fun sys_timestampString _ _ =
    KEC.LITERAL(KEC.CONSTSTR (Date.toString(Date.fromTimeLocal(Time.now()))))
    handle e => DynException.checkpoint "TimeLib.timestampString" e

fun sys_daysToString exec args = 
    case args 
     of [KEC.LITERAL (KEC.CONSTREAL r)] => 
	KEC.LITERAL(KEC.CONSTSTR (Util.daysToString(Date.fromTimeUniv(Time.fromReal(r*3600.0*24.0)))))
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
	
				       
val library = [{name="timestampSeconds", operation=sys_timestampSeconds},
	       {name="timestampString", operation=sys_timestampString},
	       {name="daysToString", operation=sys_daysToString}]
end
