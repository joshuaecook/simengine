(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

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

fun unfold pfg x =
    case pfg x
     of SOME (one, more) => one :: (unfold pfg more)
      | NONE => nil

(* Invokes a given nullary function within a specified directory, ensuring the current directory will be restored before returning. *)
fun inDir dir f =
    let
	val origDir = OS.FileSys.getDir () before OS.FileSys.chDir dir
    in
	f () before OS.FileSys.chDir origDir
	handle e => (OS.FileSys.chDir origDir; raise e)
    end
    (* OS.SysErr may be raised by various OS.FileSys functions. *)
    handle OS.SysErr (msg,err) => raise Fail ("file system error "^msg)

(* Invokes a given unary function with a distream opened for the specified directory, ensuring the stream is closed before returning. *)
fun withDirStream dirname f =
    let
	val stream = OS.FileSys.openDir dirname
    in
	f stream before OS.FileSys.closeDir stream
	handle e => (OS.FileSys.closeDir stream; raise e)
    end
    (* OS.SysErr may be raised by various OS.FileSys functions. *)
    handle OS.SysErr (msg,err) => raise Fail ("file system error "^msg)

(* Returns a (string list) of all non-hidden filenames in a directory tree rooted at dirname. *)
fun readAllDir dirname =
    let 
	(* Produces (string list option) of filename paths from a directory stream.
	 * Returns (SOME nil) if the current stream element is a "hidden" file.
	 * Returns (SOME [path]) for regular files and (SOME paths) for subdirectories.
	 * Returns (NONE) when the stream is exhausted. *)
	fun readDirstream stream =
	    case OS.FileSys.readDir stream
	     of SOME filename => 
		(* Ignore hidden files. *)
		if String.isPrefix "." filename then SOME (nil, stream)
		else
		    let 
			val path = OS.Path.joinDirFile {dir=getOpt (dirname,""), file=filename}
			    handle OS.Path.InvalidArc => raise Fail ("invalid path component file="^filename^" dir="^getOpt (dirname,""))
		    in
			(* Recursively read a subdirectory. *)
			if OS.FileSys.isDir path then SOME (readAllDir (SOME path), stream)
			else SOME ([path], stream)
		    end
	      | NONE => NONE
    in
	withDirStream (getOpt (dirname, OS.Path.currentArc)) (List.concat o (unfold readDirstream))
    end

fun getContentsFromFile filename = 
    let
	val instream = TextIO.openIn filename
    in
	TextIO.inputAll instream before TextIO.closeIn instream
    end
    handle IO.Io {name,function,cause} => raise Fail ("IO error "^name^" in function "^function)
	 | Size => raise Fail ("maximum read length exceeded")

(* Initializes the table of source code by reading files in the codegen directory tree. *)
fun init () =
    inDir "codegen/src"
	  (fn () =>
	      codeTable := foldl (fn (filename,table) =>
				     SymbolTable.enter (table, Symbol.symbol filename, 
							getContentsFromFile filename))
				 SymbolTable.empty (readAllDir NONE))
    handle Fail why => raise Fail ("failed to initialize codegen because " ^ why)

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
