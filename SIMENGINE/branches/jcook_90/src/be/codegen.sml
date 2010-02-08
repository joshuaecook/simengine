signature CODEGEN = 
sig
    (* Wrapper version of get that supplies the retrieved name in a comment and wraps the code nicely to help debugging *)
    val getC: string -> string

    (* Argument processing - replace key value pairs in output of get or getC *)
    val apply_args: (string * string) list -> string -> string
end

structure Codegen : CODEGEN =
struct

val simengine = getSIMENGINE()

val bin_path = simengine ^ "/bin/"
val lib_path = simengine ^ "/lib/"
val simlib = bin_path ^ "simlib"

val wrapper = "/******************************************************************************/\n"

fun get fname =
    let val contents =  Simlib.getContentsFromArchive {archive = (lib_path ^ "libcodegen.sim"),
						       objectName = fname}
    in
	if 0 < String.size contents then
	    contents
	else "#error file '" ^ fname ^ "' has zero length in " ^ lib_path ^ "libcodegen.sim\n"
    end
    handle Simlib.NoSuchObject => "#error file '" ^ fname ^ "' not found in " ^ lib_path ^ "libcodegen.sim\n"

fun getC fname = "/* " ^ fname ^ " */\n" ^ wrapper ^ (get fname) ^ wrapper

fun apply_args replace_rules str =
    foldl (fn((from, to),str')=> Util.repStr(str', from, to)) str replace_rules

end
