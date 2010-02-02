(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
signature SIMLIB = sig

    val makeObjectFromFile: {filename: string, objectName: string} -> string
    val makeObjectFromContents: {objectName: string, data: string} -> string
    val getFileFromArchive: {archive: string, filename: string, objectName: string} -> unit
    val getContentsFromArchive: {archive: string, objectName: string} -> string

end
