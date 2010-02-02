(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
structure Simlib: SIMLIB = struct
fun bug y = raise Fail y

type status = Int32.int

(* Nb, strings passed as parameters must be null-terminated. *)
val makeObjectFromFile' = 
    _import "simlib_MakeObjectFromFile": (string * string * string ref) -> status;

val makeObjectFromContents' =
    _import "simlib_MakeObjectFromContents": (string * int * string * string ref) -> status;

val getFileFromArchive' =
    _import "simlib_GetFileFromArchive": (string * string * string) -> status;

val getContentsFromArchive' =
    _import "simlib_GetContentsFromArchive": (string * string * string ref) -> status;

(* Appends a NULL byte to the end of a string for C compatibility. *)
fun cstring name = name ^ (str (chr (Word8.toInt 0w0)))

val error =
 fn 0 => NONE
  | 1 => SOME (Fail ("SIMLIB out of memory error"))
  | 2 => SOME (Fail ("SIMLIB error opening file"))
  | 3 => SOME (Fail ("SIMLIB error loading dynamic library"))
  | 4 => SOME (Fail ("SIMLIB object does not exist in archive"))
  | 5 => SOME (Fail ("SIMLIB compression error"))
  | 6 => SOME (Fail ("SIMLIB decompression error"))
  | n => bug ("unknown error " ^ (Int32.toString n))

fun makeObjectFromFile {filename, objectName} =
    let val objectFilename: string ref = ref ""
    in case error (makeObjectFromFile' (cstring filename, cstring objectName, objectFilename))
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
