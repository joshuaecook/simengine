structure FileSysLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_usererror [PosLog.NOPOS] (Printer.$ msg)

fun std_pwd exec args =
    KEC.LITERAL(KEC.CONSTSTR (Directory.pwd()))

fun std_chmod exec args =
    KEC.UNIT

fun std_getPermissions exec args =
    KEC.UNIT

fun std_rmfile exec args =
    KEC.UNIT

fun std_rmdir exec args =
    KEC.UNIT

fun std_chdir exec args =
    KEC.UNIT

fun std_ls exec args =
(*    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	let
	    
	in
	    KEC.list2kecvector (map buildentry (Directory.dir2files dir))
	end
      | 
*)
KEC.UNIT 

fun std_isdir exec args =
    KEC.UNIT

fun std_isfile exec args =
    KEC.UNIT

fun std_mkdir exec args =
    KEC.UNIT

fun std_realpath exec args =
    KEC.UNIT

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
