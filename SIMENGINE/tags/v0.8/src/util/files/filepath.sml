structure FilePath =
struct

exception InternalError

fun getfullpath file =
    let
	val file' = OS.FileSys.fullPath file
    in
	SOME file'
    end
    handle _ => NONE

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
