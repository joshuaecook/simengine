(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
signature SIMLIB = sig

    exception NoSuchObject
    exception Compression

    val makeObjectFromFile: {objectName: string, filename: string} -> string
    val makeObjectFromContents: {objectName: string, data: string} -> string
    val getFileFromArchive: {archive: string, objectName: string, filename: string} -> unit
    val getContentsFromArchive: {archive: string, objectName: string} -> string

end
