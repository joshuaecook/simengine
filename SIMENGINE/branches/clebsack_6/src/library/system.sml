structure SystemLib =
struct

fun sys_copyright _ args = KEC.LITERAL (KEC.CONSTSTR Globals.copyright)
fun sys_version _ args = KEC.LITERAL (KEC.CONSTSTR Globals.version)
fun sys_build _ args = KEC.LITERAL (KEC.CONSTSTR BuildOptions.build)
fun sys_build_date _ args = KEC.LITERAL (KEC.CONSTSTR BuildOptions.build_date)

fun sys_path _ args = KEC.list2kecvector (map ((fn (s) => KEC.LITERAL (KEC.CONSTSTR s)))
					      ((!ParserSettings.filepath) :: 
					       map StdFun.expand_env_variables 
						   (DynamoOptions.getStringVectorSetting("sourcepath"))))
				       
val library = [{name="sys_copyright", operation=sys_copyright},
	       {name="sys_version", operation=sys_version},
	       {name="sys_build", operation=sys_build},
	       {name="sys_build_date", operation=sys_build_date},
	       {name="sys_path", operation=sys_path}]
end
