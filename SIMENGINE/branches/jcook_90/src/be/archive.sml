signature ARCHIVE = 
sig
    (* Returns the contents of an archived file given the name *)
    val get: string -> string
    (* Returns the contents of multiple files given their names *)
    val mget: string list -> string list
    (* Wrapper version of get that supplies the retrieved name in a comment and wraps the code nicely to help debugging *)
    val getC: string -> string
    (* Puts the contents of a named file into an archivable object and returns the object name *)
    val put: string -> string
    (* Puts the contents of all named fiels into archivable objects and returns the object names *)
    val mput: string list -> string list

    (* Argument processing - replace key value pairs in output of get or getC *)
    val apply_args: (string * string) list -> string -> string

end

structure Archive : ARCHIVE =
struct

val simengine = 
    case (OS.Process.getEnv "SIMENGINE") of
	SOME x => x
      | NONE => "./"

val bin_path = simengine ^ "/bin/"
val lib_path = simengine ^ "/lib/"
val simlib = bin_path ^ "simlib"

val wrapper = "/******************************************************************************/\n"

fun readfileString pipein =
    case (TextIO.inputLine pipein) of
		SOME x => x ^ (readfileString pipein)
	      | NONE => ""

fun readfileList pipein =
    case (TextIO.inputLine pipein) of
		SOME x => x :: (readfileList pipein)
	      | NONE => []

fun get fname =
    let
	val clargs = ["SiMagic", "get", lib_path ^ "libcodegen.sim", fname]
	val proc = 
	    MLton.Process.create{args = clargs,
				 env = NONE,
				 path = simlib,
				 stderr = MLton.Process.Param.null,
				 stdin = MLton.Process.Param.null,
				 stdout = MLton.Process.Param.pipe}
	val pipein = MLton.Process.Child.textIn (MLton.Process.getStdout proc)
	val file = readfileString pipein
	val _ = MLton.Process.reap proc
    in
	file
    end

fun mget fnames =
    map get fnames

fun getC fname = "/* " ^ fname ^ " */\n" ^ wrapper ^ (get fname) ^ wrapper

fun mput fnames =
    let
	val clargs = ["SiMagic", "put", lib_path ^ "libcodegen.*"] @ fnames
	val proc = 
	    MLton.Process.create{args = clargs,
				 env = NONE,
				 path = simlib,
				 stderr = MLton.Process.Param.null,
				 stdin = MLton.Process.Param.null,
				 stdout = MLton.Process.Param.pipe}
	val pipein = MLton.Process.Child.textIn (MLton.Process.getStdout proc)
	val files = readfileList pipein
	val _ = MLton.Process.reap proc
    in
	files
    end

fun put fname =
    List.hd (mput [fname])

fun apply_args replace_rules str =
    foldl (fn((from, to),str')=> Util.repStr(str', from, to)) str replace_rules

end
