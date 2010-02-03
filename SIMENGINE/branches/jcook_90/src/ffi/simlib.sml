(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
structure Simlib: SIMLIB = struct
fun bug y = raise Fail y

type status = Int32.int

(* Nb, strings passed as parameters must be null-terminated. *)
val makeObjectFromFile' = 
    _import "simlib_MakeObjectFromFile": (string * string * string ref) -> status;

val makeObjectFromContents' =
    _import "simlib_MakeObjectFromContents": (string * Int32.int * string * string ref) -> status;

val getFileFromArchive' =
    _import "simlib_GetFileFromArchive": (string * string * string) -> status;

val getContentsFromArchive' =
    _import "simlib_GetContentsFromArchive": (string * string * string ref) -> status;

val errno =
    _import "simlib_errno": (unit) -> Int32.int;

val strerror =
    _import "simlib_strerror": (Int32.int) -> string;

(* Appends a NULL byte to the end of a string for C compatibility. *)
fun cstring name = name ^ (str (chr (Word8.toInt 0w0)))

exception NoSuchObject
exception Compression

val error =
 fn 0 => NONE
  | 1 => SOME (Fail ("Simlib out of memory"))
  | 2 => 
    let val eid = errno ()
    in
	SOME (IO.Io {name = "Simlib", function = "fopen", 
		     cause = OS.SysErr (strerror eid, NONE)})
    end
  | 3 => 
    let val eid = errno ()
    in
	SOME (IO.Io {name = "Simlib", function = "dlopen", 
		     cause = OS.SysErr (strerror eid, NONE)})
    end
  | 4 => SOME NoSuchObject
  | 5 => SOME Compression
  | 6 => SOME Compression
  | n => bug ("Simlib unknown error " ^ (Int32.toString n))

fun makeObjectFromFile {filename, objectName} =
    let val objectFilename: string ref = ref ""
    in case error (makeObjectFromFile' (cstring objectName, cstring filename, objectFilename))
	of NONE => ! objectFilename
	 | SOME exn => raise exn
    end

fun makeObjectFromContents {objectName, data} =
    let val objectFilename: string ref = ref ""
    in case error (makeObjectFromContents' (cstring objectName, String.size data, data, objectFilename))
	of NONE => ! objectFilename
	 | SOME exn => raise exn
    end

fun getFileFromArchive {archive, filename, objectName} =
    case error (getFileFromArchive' (cstring archive, cstring objectName, cstring filename))
     of NONE => ()
      | SOME exn => raise exn

fun getContentsFromArchive {archive, objectName} =
    let val data: string ref = ref ""
    in case error (getContentsFromArchive' (cstring archive, cstring objectName, data))
	of NONE => ! data
	 | SOME exn => raise exn
    end


end
