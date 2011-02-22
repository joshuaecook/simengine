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
