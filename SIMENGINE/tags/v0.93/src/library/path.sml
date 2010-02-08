(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
structure PathLib = struct
(* See the OS.Path structure from standard basis. *)

val strfun = LibraryUtil.strfun
val strsfun = LibraryUtil.strsfun

(* Returns the directory part of a path string. *)
fun dir exec = strfun OS.Path.dir
(* Returns the filename part of a path string. *)
fun file exec = strfun OS.Path.file
(* Concatenates a directory and filename with the appropriate separator. *)
fun join exec = strsfun (fn (d,f) => OS.Path.joinDirFile {dir=d, file=f})
(* Returns the base name of a path string. *)
fun base exec = strfun OS.Path.base
(* Returns the file extension of a path string. *)
fun ext exec = strfun (fn p => case OS.Path.ext p of SOME e => e | _ => "")

val library = [{name="path_dir", operation=dir},
	       {name="path_file", operation=file},
	       {name="path_join", operation=join},
	       {name="path_base", operation=base},
	       {name="path_ext", operation=ext}]
end
