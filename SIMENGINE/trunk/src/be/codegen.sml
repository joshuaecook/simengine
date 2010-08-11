signature CODEGEN = 
sig
    (* Initializes the codeTable with C source located in codegen/src *)
    val init: unit -> unit
    (* Wrapper version of get that supplies the retrieved name in a comment and wraps the code nicely to help debugging *)
    val getC: string -> string

    (* Argument processing - replace key value pairs in output of get or getC *)
    val apply_args: (string * string) list -> string -> string
end

structure Codegen : CODEGEN =
struct

val codeTable: string SymbolTable.table ref = ref SymbolTable.empty

fun init () =
    let
	val codegenDir = OS.Path.joinDirFile {dir="codegen", file="src"}
	(* Read all codegen file names *)
	fun readAllDir dirname = 
	    let
		val origDir = OS.FileSys.getDir()
		val _ = if dirname = "" then 
			    OS.FileSys.chDir codegenDir
			else
			    OS.FileSys.chDir dirname
		val dirstream = OS.FileSys.openDir "."
		(* Recurse through the current directory *)
		fun readAllDirstream dirstream =
		    case OS.FileSys.readDir dirstream of
			SOME filename => 
			let
			    val relativefilename = (OS.Path.joinDirFile {dir=dirname, file=filename})
			in
			    if String.isPrefix "." filename then
				(* Skip any '.' files and directories *)
				readAllDirstream dirstream
			    else
				if OS.FileSys.isDir filename then
				    (* Recurse through subdirectories *)
				    (readAllDir filename) @ (readAllDirstream dirstream)
				else
				    relativefilename :: (readAllDirstream dirstream)
			end
		      | NONE => ([] before OS.FileSys.closeDir dirstream)
	    in
		readAllDirstream dirstream before OS.FileSys.chDir origDir
	    end

	(* Insert the contents of all codegen files into table associated by name *)
	fun getContentsFromFile filename = 
	    let
		val fullfilename = OS.Path.concat (codegenDir, filename)
		val instream = TextIO.openIn fullfilename
	    in
		TextIO.inputAll instream before TextIO.closeIn instream
	    end

    in
	codeTable := foldl (fn(filename , table) =>  SymbolTable.enter (table, (Symbol.symbol filename), (getContentsFromFile filename))) 
			   SymbolTable.empty (readAllDir "")
    end

fun getC filename =
    let
	val wrapper = "/******************************************************************************/\n"
    in
	case (SymbolTable.look (!codeTable, (Symbol.symbol filename))) of
	    SOME contents => "/* " ^ filename ^ " */\n" ^ wrapper ^ contents ^ wrapper
	  | NONE => "#error file '" ^ filename ^ "' not found in codegen library\n"
    end

fun apply_args replace_rules str =
    foldl (fn((from, to),str')=> Util.repStr(str', from, to)) str replace_rules
end
