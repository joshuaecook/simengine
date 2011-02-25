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

structure FilePath =
struct

exception InternalError

fun getfullpath file =
    let
	val file' = OS.FileSys.fullPath file

    in
	SOME file'
    end
    handle OS.SysErr (s, _) => NONE 

fun find file =
    if OS.Path.isAbsolute file then
     fn _ => getfullpath file
    else
	let fun findin nil = NONE
	      | findin (dir :: dirs) =
		(case getfullpath (OS.Path.concat (dir, file)) of
		    SOME path => SOME path
		  | NONE => findin dirs)
	in findin
	end

(* takes in a list of directories and a filename and finds the
absolute path to the first dir/file that exists *)
fun abs_path nil filename =
    DynException.stdException ("Cannot find file '" ^ filename ^ "' in any path", "FilePath.abs_path", Logger.USER) 
  | abs_path (dir::dirs) filename =
    (case getfullpath (OS.Path.joinDirFile {dir=dir,file=filename}) of
	 SOME path => path
       | NONE => abs_path dirs filename)


end
