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

(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
structure PathLib = struct
(* See the OS.Path structure from standard basis. *)

val ValueError = DynException.ValueError

val strfun = LibraryUtil.strfun
val strsfun = LibraryUtil.strsfun

(* Returns the directory part of a path string. *)
fun dir exec = strfun OS.Path.dir
(* Returns the filename part of a path string. *)
fun file exec = strfun OS.Path.file
(* Concatenates a directory and filename with the appropriate separator. *)
fun join exec = strsfun (fn (dir, file) => 
			    OS.Path.concat (dir, file)
			    handle OS.Path.Path => 
				   raise ValueError ("Invalid filename " ^ file)
				 | Size => 
				   raise ValueError ("Filename is too large"))
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
