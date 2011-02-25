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

structure EnvironmentLib =
struct

val getEnv = OS.Process.getEnv
val setEnv = MLton.ProcEnv.setenv

val TypeMismatch = DynException.TypeMismatch
and ValueError = DynException.ValueError
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun env_getVar _ args = 
    case args of
	[KEC.LITERAL (KEC.CONSTSTR name)] =>
	(case getEnv name of
	     SOME var => KEC.LITERAL (KEC.CONSTSTR var)
	   | NONE => KEC.UNDEFINED)
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


fun env_setVar _ args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR name), KEC.LITERAL (KEC.CONSTSTR value)] =>
	KEC.UNIT before
	setEnv {name=name, value=value}
      | [a, b] => raise TypeMismatch ("expected 2 strings but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

fun env_getCommandLine exec args =
    exec (KEC.list2kecvector (map (KEC.LITERAL o KEC.CONSTSTR) (CommandLine.arguments ())))
		      
val library = [{name="getEnv", operation=env_getVar},
	       {name="setEnv", operation=env_setVar},
	       {name="getCommandLine", operation=env_getCommandLine}]
	      
end
