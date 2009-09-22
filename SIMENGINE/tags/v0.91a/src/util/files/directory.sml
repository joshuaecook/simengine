(* directory management routines *)

signature DIRECTORY =
sig
    val pwd : unit -> string
    val chmod : string * Posix.FileSys.S.flags -> unit
    val chDir : string -> unit
    val isDir : string -> bool
    val isFile : string -> bool
    val isLink : string -> bool
    val dir2files : string -> string list
    val rmFile : string -> unit
    val rmDir : string -> unit
    val mkDir : string -> unit
end

structure Directory : DIRECTORY =
struct

exception FileIOError;

open Printer

(* new functions with checkpoints.  Potential errors should be logged before these exceptions are raised,
 but this is a last ditch effort to handle errors appropriately and with helpful logging.*)
fun pwd () = 
    OS.FileSys.getDir ()
    handle e => DynException.checkpoint ("Directory.pwd") e

fun chmod (file, permission) =
    Posix.FileSys.chmod (file, permission)
    handle e => DynException.checkpoint ("Directory.chmod ('"^file^"')") e

fun OSremove file =
    OS.FileSys.remove file
    handle e => DynException.checkpoint ("Directory.OSremove ('"^file^"')") e

fun OSrmDir dir =
    OS.FileSys.rmDir dir
    handle e => DynException.checkpoint ("Directory.OSrmDir ('"^dir^"')") e

fun chDir dir =
    OS.FileSys.chDir dir
    handle e => DynException.checkpoint ("Directory.chDir ('"^dir^"')") e

fun openDir dir =
    OS.FileSys.openDir dir
    handle e => DynException.checkpoint ("Directory.openDir ('"^dir^"')") e

fun closeDir dirstream =
    OS.FileSys.closeDir dirstream
    handle e => DynException.checkpoint ("Directory.closeDir") e

fun readDir dirstream =
    OS.FileSys.readDir dirstream
    handle e => DynException.checkpoint ("Directory.readDir") e

(* check if directory exists *)
fun isDir dir =
    OS.FileSys.isDir dir
    handle _ => false

(* check file if it exists - this function returns false on symbolic links *)
fun isFile file =
    (OS.FileSys.fileSize file;
     if isDir file then
	 false
     else 
	 true)
    handle _ => false

(* check to see if it is a link *)
fun isLink file = 
    (Posix.FileSys.readlink file;
     true)
    handle _ => false

(* returns a string listing of all files/directories in the current directory*)
fun dir2files_helper dirstream =
    (case (readDir dirstream) of
	 SOME v => v::(dir2files_helper dirstream)
       | NONE => nil)
    handle e => DynException.checkpoint "Directory.dir2files_helper" e

fun dir2files dir = 
    let
	val dirstream = openDir dir
	val _ = Posix.FileSys.rewinddir dirstream (* must rewind since readDir has side effects *)
	val files = dir2files_helper dirstream
	val _ = closeDir dirstream
    in
	files
    end
    handle e => DynException.checkpoint ("Directory.dir2files ('"^dir^"')") e

fun isEmpty dir =
    (if isDir dir then
	 List.length (dir2files dir) = 0
     else
	 DynException.stdException (("Trying to query a directory that isn't a directoy ('"^dir^"')"), "Directory.isEmpty", Logger.INTERNAL))
    handle e => DynException.checkpoint ("Directory.isEmpty ('"^dir^"')") e
	 

(* removes a file from the system *)
fun rmFile file = 
    	(if isFile file then 
	     (chmod (file, Posix.FileSys.S.irwxu);
	     OSremove file;
	      if isFile file then
		  (Logger.log_error ($("Attempted to remove file '"^file^"' but failed"));
		   DynException.setErrored())
	      else())
	 else if isLink file then
	     (OSremove file;
	      if isLink file then
		  (Logger.log_error ($("Attempted to remove link '"^file^"' but failed"));
		   DynException.setErrored())
	      else
		  ())
	 else
	     Logger.log_warning ($("Can't remove file '"^file^"' that doesn't exist")))
    handle e => (Logger.log_error ($("Can't modify file '"^file^"' for unknown reason"));
		 DynException.setErrored())


(* Removes a full directory tree including all subdirectories *)
fun rmDir dir =
    (if isDir dir then
	 if isEmpty dir then
	     (chmod (dir, Posix.FileSys.S.irwxu);
	      OSrmDir(dir))
	     handle _ => (Logger.log_error ($("Can't remove empty directory '"^dir^"'"));
			  DynException.setErrored())
	 else
	     let
		 val files = dir2files dir
		 val curDir = pwd()
		 val _ = chDir(dir)
		 val _ = app (fn(d)=>if isDir d then
					 rmDir d
				     else
					 if isFile d then 
					     rmFile d
					 else if isLink d then
					     rmFile d
					 else
					     (Logger.log_error ($("Can't remove '"^d^"' from filesystem"));
					      DynException.setErrored()))
			     (files)
		 val _ = chDir(curDir)
			 
		 val _ =  if isDir dir andalso isEmpty dir then
			      OSrmDir(dir) handle _ => (Logger.log_error ($("Directory '"^dir^"' is empty but can not be removed"));
							DynException.setErrored())
			  else
			      if isEmpty dir then 
				  (Logger.log_error ($("Directory '"^dir^"' can not be deleted"));
				   DynException.setErrored())
			      else
				  (Logger.log_error ($("Can't delete directory '"^dir^"' because it is not empty"));
				   DynException.setErrored())
	     in
		 ()
	     end
     else
	 (Logger.log_error($("Directory '"^dir^"' is attempting to be removed but does not exist"));
	  DynException.setErrored();
	  DynException.checkToProceed()))
    handle e => DynException.checkpoint ("Directory.rmDir ('"^dir^"')") e

(* Create the directory and IMPORTANT - remove all files if it exists *)
fun mkDir dir =
    let
	val _ = if isDir dir then
		    if DynamoOptions.isFlagSet "force" then
			(Logger.log_notice ($("Overwriting directory '"^dir^"'"));
			 rmDir dir;
			 OS.FileSys.mkDir(dir) handle _ => (Logger.log_error ($("Can't create directory '"^dir^"'"));
							    DynException.setErrored()))
		    else
			(Logger.log_error ($("Directory '"^dir^"' exists, use +f or +force to overwrite"));
			 DynException.setErrored())
		else
		    (Logger.log_notice ($("Creating directory '"^dir^"'"));
		     OS.FileSys.mkDir(dir) handle _ => (Logger.log_error ($("Can't create directory '"^dir^"'"));
							DynException.setErrored()))
    in
	DynException.checkToProceed()	
    end
    handle e => DynException.checkpoint ("Directory.mkDir (" ^ "'" ^ dir ^ "'" ^ ")") e

end
