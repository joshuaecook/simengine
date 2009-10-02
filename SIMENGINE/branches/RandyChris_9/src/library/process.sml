structure ProcessLib =
struct

val TypeMismatch = DynException.TypeMismatch
and ValueError = DynException.ValueError
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_usererror [PosLog.NOPOS] (Printer.$ msg)


fun std_popen exec args =
    case args of
	[a as KEC.LITERAL (KEC.CONSTSTR str), b as KEC.VECTOR vec] => 
	let
	     open MLton.Process

	     fun kecstr2str (KEC.LITERAL(KEC.CONSTSTR s)) = s
	       | kecstr2str exp = 
		 PrettyPrint.kecexp2prettystr exec exp

	     val args = map kecstr2str (KEC.kecvector2list vec)

	     val file = FilePath.find str (!Globals.path)

	     val proc = 
		 create {args = args,
			 env = SOME (Posix.ProcEnv.environ()),
			 path = valOf file,
			 stderr = Param.pipe,
			 stdin = Param.pipe,
			 stdout = Param.pipe}
	 in
	     KEC.PROCESS (proc, valOf file, args)
	 end
		
      | [a, b] =>
	raise TypeMismatch ("expected a string and a list of strings, but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

fun std_preadline exec args =
    case args of
	[KEC.PROCESS (p, _, _)] =>
	(case TextIO.inputLine (MLton.Process.Child.textIn (MLton.Process.getStdout p)) of
	    NONE => KEC.UNIT
	  | SOME s => KEC.LITERAL(KEC.CONSTSTR s))
      | [a] => 
	raise TypeMismatch ("expected a process, but received " ^ (PrettyPrint.kecexp2nickname a))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_pwrite exec args =
    case args of
	[KEC.PROCESS (p, _, _), KEC.LITERAL(KEC.CONSTSTR s)] =>
	(TextIO.output (MLton.Process.Child.textOut(MLton.Process.getStdin p), s);
	 KEC.UNIT)
      | [a, b] => 
	raise TypeMismatch ("expected a process and string, but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

fun std_preap exec args =
    case args of
	[KEC.PROCESS (p, _, _)] =>
	(MLton.Process.reap p;
	 KEC.UNIT)
      | [a] => 
	raise TypeMismatch ("expected a process, but received " ^ (PrettyPrint.kecexp2nickname a))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="popen", operation=std_popen},
	       {name="preadline", operation=std_preadline},
	       {name="pwrite", operation=std_pwrite},
	       {name="preap", operation=std_preap}]

end
