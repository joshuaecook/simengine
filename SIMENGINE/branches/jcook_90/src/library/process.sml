structure ProcessLib =
struct

val TypeMismatch = DynException.TypeMismatch
and ValueError = DynException.ValueError
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

structure Proc = MLton.Process
local open Proc
in
structure Param = Param
structure Child = Child
end

fun error msg =
    Logger.log_error (Printer.$ msg)


fun std_popen exec args =
    case args of
	[a as KEC.LITERAL (KEC.CONSTSTR str), b as KEC.VECTOR vec] => 
	let
	     fun kecstr2str (KEC.LITERAL(KEC.CONSTSTR s)) = s
	       | kecstr2str exp = 
		 PrettyPrint.kecexp2prettystr exec exp

	     val args = map kecstr2str (KEC.kecvector2list vec)

	     val file = 
		 case FilePath.find str (!Globals.path)
		  of SOME f => f
		   | NONE => raise ValueError ("Cannot find executable " ^ str)

	     val proc = 
		 Proc.create {args = args,
			      env = SOME (Posix.ProcEnv.environ()),
			      path = file,
			      stderr = Param.pipe,
			      stdin = Param.pipe,
			      stdout = Param.pipe}
		 handle e => raise ValueError ("Error opening process "^str)
	 in
	     KEC.PROCESS (proc, file, args)
	 end
		
      | [a, b] =>
	raise TypeMismatch ("expected a string and a list of strings, but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

fun std_preadline exec args =
    case args of
	[KEC.PROCESS (p, _, _)] =>
	let val stdout = Child.textIn (Proc.getStdout p)
	in case TextIO.inputLine stdout
	    of SOME s => KEC.LITERAL (KEC.CONSTSTR s)
	     | NONE => KEC.UNIT
	end
      | [a] => 
	raise TypeMismatch ("expected a process, but received " ^ (PrettyPrint.kecexp2nickname a))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_preaderrline exec args =
    case args of
	[KEC.PROCESS (p, _, _)] =>
	let val stderr = Child.textIn (Proc.getStderr p)
	in case TextIO.inputLine stderr
	    of SOME s => KEC.LITERAL (KEC.CONSTSTR s)
	     | NONE => KEC.UNIT
	end
      | [a] => 
	raise TypeMismatch ("expected a process, but received " ^ (PrettyPrint.kecexp2nickname a))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_preadOutAndErrLine exec args =
    case args
     of [KEC.PROCESS (p, name, args)] =>
	let val (stdout, stderr) = (Child.textIn (Proc.getStdout p), Child.textIn (Proc.getStderr p))
	    fun loop (count) =
		case (TextIO.canInput (stdout,10), TextIO.canInput (stderr,10))
		 of 
		    (SOME x, SOME y) => 
		    if count = 0 andalso x = 0 andalso y = 0 then
			(Posix.Process.sleep (Time.fromMilliseconds 10); loop (count-1))
		    else
			(TextIO.inputLine stdout, TextIO.inputLine stderr)
		  | (SOME x, NONE) => 
		    if count = 0 andalso x = 0 then
			(Posix.Process.sleep (Time.fromMilliseconds 10); loop (count-1))
		    else
			(TextIO.inputLine stdout, NONE)
		  | (NONE, SOME x) => 
		    if count = 0 andalso x = 0 then
			(Posix.Process.sleep (Time.fromMilliseconds 10); loop (count-1))
		    else
			(NONE, TextIO.inputLine stderr)
		  (* If no data is available, it can't hurt to sleep for a short interval to avoid pegging the cpu *)
		  | (NONE, NONE) => (Posix.Process.sleep (Time.fromMilliseconds 10); loop (count))

	    val (outline, errline) = loop (10)
	in
	    KEC.TUPLE [if isSome outline then 
			   KEC.LITERAL (KEC.CONSTSTR (valOf outline))
		       else KEC.UNIT,
		       if isSome errline then 
			   KEC.LITERAL (KEC.CONSTSTR (valOf errline))
		       else KEC.UNIT]
	end
      | [a] => raise TypeMismatch ("expected a process, but received " ^ (PrettyPrint.kecexp2nickname a))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_pwrite exec args =
    case args of
	[KEC.PROCESS (p, _, _), KEC.LITERAL(KEC.CONSTSTR s)] =>
	(TextIO.output (Child.textOut(Proc.getStdin p), s);
	 KEC.UNIT)
      | [a, b] => 
	raise TypeMismatch ("expected a process and string, but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* See http://mlton.org/pipermail/mlton-user/2009-April/001521.html
 * for an explanation of the return status of MLton.Process.reap. *)
fun std_preap exec args =
    case args of
	[KEC.PROCESS (p, _, _)] =>
	(case Proc.reap p
	  of Posix.Process.W_EXITED => KEC.LITERAL ( KEC.CONSTREAL 0.0)
	   | Posix.Process.W_EXITSTATUS w => (KEC.LITERAL o KEC.CONSTREAL o Real.fromInt o Word8.toInt) w
	   | _ => KEC.UNIT)
      | [a] => 
	raise TypeMismatch ("expected a process, but received " ^ (PrettyPrint.kecexp2nickname a))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="popen", operation=std_popen},
	       {name="preadline", operation=std_preadline},
	       {name="preaderrline", operation = std_preaderrline},
	       {name="preadOutAndErrLine", operation = std_preadOutAndErrLine},
	       {name="pwrite", operation=std_pwrite},
	       {name="preap", operation=std_preap}]

end
