signature ARCHIVE = sig

exception Invalid
(* A valid archive must have a well-formed manifest.
 * Manifest data is currently stored as a JSON object at least with
 * these members: 
 *
 * "version": an integer.
 * "creationDate": an integer (Unix) timestamp.
 * "dolFilename": a pathname string.
 * "dslFilenames": an array of pathname strings, where the first
 * 	element is an absolute path addressing the main model file,
 * 	and subsequent elements are the relative paths of imported files.
 * "environment": an object whose members represent the compile-time
 * 	user environment.
 * "executables": an array of ??? representing compiled
 * 	simulations. Currently, this array must have a single element.
 *)

type archive

(* Opens an existing archive with a given filename.
 * Fails with an IO.Io exception if a file with that name doesn't exist
 * or an Invalid exception if the file is not a valid archive. *)
val openArchive: string -> archive
(* Closes an open archive. 
 * If the archive contents have been modified, it will be recreated to 
 * reflect the modifications.
 * Modifications made to an archive will only be visible to subsequent
 * openings after the modified archive is closed.
 *)
val close: archive -> unit

(* Creates a new archive with a given filename. 
 * Fails with an IO.Io exception if a file with that name already exists. *)
val new: {filename: string,
	  dolFilename: string,
	  dslFilenames: string list,
	  environment: (string * string) list,
	  executable: Manifest.executable} -> archive

(* Closes an open archive and destroys the files associated with it. *)
val destroy: archive -> unit

(* Returns the time at which the archive was created. *)
val creationDate: archive -> Time.time

(* Returns the version id of the archive. 
 * Archives with differing versions are not ensured to be compatible.
 *)
val version: archive -> int

(* Returns the pathname of the DOL settings file used when compiling
 * this model. *)
val dolFilename: archive -> string

(* Returns the list of pathnames to all files making up this model.
 * The first element will be an absolute path to the main DSL file.
 * Other elements are relative paths of imported files. 
 *)
val dslFilenames: archive -> string list

(* Applies a predicate function to each executable in the manifest,
 * until the predicate is satisfied or all executables have been
 * evaluated.
 * Returns SOME (executable) for the executable satisfying the
 * predicate or NONE if the predicate could not be satisfied.
 *)
val findExecutable: (Manifest.executable -> bool) -> archive -> Manifest.executable option


end

