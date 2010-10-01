(* This module contains interative shell display routines *)

structure ShellPrint =
struct

(* Print the results of an execution to the shell display.  This should be called whenever a shell command is executed interactively*)
fun showResult exec exp =
    let
	val pretty = PrettyPrint.kecexp2prettystr exec
    in
	print ("::> " ^ (pretty exp) ^ "\n")
    end
end
