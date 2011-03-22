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

signature PROCESS =
sig

    exception ProcessError

    (* take in a command and arguments and return an error code and a string list representing standard out *)
    val system : (string * string list) -> (int * string list)

end
structure Process : PROCESS =
struct

structure Proc = MLton.Process
local open Proc
in
structure Param = Param
structure Child = Child
end

(* Define a specific exception *)
exception ProcessError		 

(* system - run a process, return the output from standard out in a list of strings *)
(* cmd must be absolute path to command*)
fun system (cmd, args) =
    let
	val proc = 
	    Proc.create {args = args,
			 env = SOME (Posix.ProcEnv.environ()),
			 path = cmd,
			 stderr = Param.pipe,
			 stdin = Param.pipe,
			 stdout = Param.pipe}
	    handle e => raise ProcessError

	val stdout = Child.textIn (Proc.getStdout proc)

	fun readLine () =
	    case TextIO.inputLine stdout of
		SOME s => (StdFun.dropNewLine s)::(readLine())
	      | NONE => []

	val text = readLine()
			      
	val status = case Proc.reap proc of
			 Posix.Process.W_EXITED => 0
		       | Posix.Process.W_EXITSTATUS w => Word8.toInt w
		       | _ => ~1
    in
	(status, text)
    end
    handle e => DynException.checkpoint "Process.system" e

end
