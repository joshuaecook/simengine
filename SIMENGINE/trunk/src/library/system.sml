structure SystemLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun sys_startupMessage _ args = KEC.LITERAL (KEC.CONSTSTR (Globals.startupMessage()))
fun sys_copyright _ args = KEC.LITERAL (KEC.CONSTSTR Globals.copyright)
fun sys_version _ args = KEC.LITERAL (KEC.CONSTSTR Globals.version)
fun sys_build _ args = KEC.LITERAL (KEC.CONSTSTR BuildOptions.build)
fun sys_build_date _ args = KEC.LITERAL (KEC.CONSTSTR (Globals.buildDate()))
fun sys_build_number _ args = KEC.LITERAL (KEC.CONSTREAL (Real.fromInt BuildOptions.buildNumber))
fun sys_build_time _ args = KEC.LITERAL (KEC.CONSTREAL (Time.toReal (BuildOptions.buildTime)))
fun sys_architecture _ args = KEC.LITERAL (KEC.CONSTSTR BuildOptions.architecture)

fun sys_path exec args = 
    exec (KEC.list2kecvector (map ((fn (s) => KEC.LITERAL (KEC.CONSTSTR s)))
				  ((!ParserSettings.filepath) :: 
				   map StdFun.expand_env_variables 
				       (DynamoOptions.getStringVectorSetting("sourcepath")))))

fun sys_exit _ args =
    (case args of
	 [KEC.LITERAL(KEC.CONSTREAL (r))] => 
	 let
	     val i = Real.floor r
	     val _ = if i < 0 orelse i > 255 then
			 raise DynException.TypeMismatch ("Expecting 8-bit integer, but received "^(StdFun.real2str r))
		     else 
			 ()
	     val _ = Posix.Process.exit (Word8.fromInt i)
	 in
	     KEC.LITERAL(KEC.CONSTREAL (r))
	 end
       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})
    handle e => DynException.checkpoint "SystemLib.sys_exit" e

				       
val library = [{name="sys_startupMessage", operation=sys_startupMessage},
	       {name="sys_copyright", operation=sys_copyright},
	       {name="sys_version", operation=sys_version},
	       {name="sys_build", operation=sys_build},
	       {name="sys_build_number", operation=sys_build_number},
	       {name="sys_build_date", operation=sys_build_date},
	       {name="sys_build_time", operation=sys_build_time},
	       {name="sys_architecture", operation=sys_architecture},
	       {name="sys_exit", operation=sys_exit},
	       {name="sys_path", operation=sys_path}]
end
