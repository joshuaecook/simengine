structure FileSysLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_usererror [PosLog.NOPOS] (Printer.$ msg)

fun std_pwd exec args =
    KEC.LITERAL(KEC.CONSTSTR (Directory.pwd()))

fun std_chmod exec args =
    let
    in
	KEC.UNIT
    end

fun std_getPermissions exec args =
    KEC.UNIT

fun std_rmfile exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR file)] =>
	KEC.UNIT before Directory.rmFile file
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_rmdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.UNIT before Directory.rmDir dir
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_chdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.UNIT before Directory.chDir dir
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
    

fun std_ls exec args =
    case args of
	nil =>
	let
	    fun buildentry file =
		KEC.LITERAL(KEC.CONSTSTR file)
	in
	    exec(KEC.list2kecvector (map buildentry (Directory.dir2files ".")))
	end
      | [KEC.LITERAL(KEC.CONSTSTR dir)] =>
	(let
	     val _ = print ("entering ls\n")
	     fun buildentry file =
		 KEC.LITERAL(KEC.CONSTSTR file)
	     val files = Directory.dir2files dir
	     val _ = print ("  files = " ^ (String.concatWith ", " files) ^ "\n")
	 in
	     exec(KEC.list2kecvector (map buildentry (files)))
	 end
	 handle _ => (print ("caught an exception\n");
		      KEC.UNIT))
      | [arg] 
	=> raise TypeMismatch ("expected no arguments or a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


fun std_isdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.LITERAL(KEC.CONSTBOOL (Directory.isDir dir))
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
    

fun std_isfile exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR file)] =>
	KEC.LITERAL(KEC.CONSTBOOL (Directory.isFile file))
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_mkdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.UNIT before Directory.mkDir dir
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_realpath exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR file)] =>
	(case FilePath.getfullpath file of
	     SOME str =>
	     KEC.LITERAL(KEC.CONSTSTR(str))
	   | NONE => KEC.UNIT)
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="pwd", operation=std_pwd},
	       {name="chmod", operation=std_chmod},
	       {name="getPermissions", operation=std_getPermissions},
	       {name="rmfile", operation=std_rmfile},
	       {name="rmdir", operation=std_rmdir},
	       {name="chdir", operation=std_chdir},
	       {name="ls", operation=std_ls},
	       {name="isdir", operation=std_isdir},
	       {name="isfile", operation=std_isfile},
	       {name="mkdir", operation=std_mkdir},
	       {name="realpath", operation=std_realpath}]

end
