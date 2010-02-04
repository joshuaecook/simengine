structure Archive: ARCHIVE = struct

fun bug y = raise Fail y




exception Invalid


datatype archive 
  = A of {dirty: bool ref,
	  filename: string,
	  manifest: Manifest.manifest,
	  workingPath: string}
	 

local
    fun access f (A r) = f r
in
val manifest = access #manifest
val version = Manifest.version o manifest
val creationDate = Manifest.creationDate o manifest
val dolFilename = Manifest.dolFilename o manifest
val dslFilenames = Manifest.dslFilenames o manifest
end



fun openArchive filename =
	let 
	    val manifest'json = 
		Simlib.getContentsFromArchive {archive = filename, 
					       objectName = "MANIFEST.json"}
		handle Simlib.NoSuchObject => raise Invalid
		     | Simlib.Compression => raise Invalid

	    val manifest = Manifest.fromJSON (ParseJSON.parseString manifest'json)
		handle exn => raise Invalid
	in
	    A {filename = filename,
	       dirty = ref false,
	       manifest = manifest,
	       workingPath = ""}
	    handle Option => raise Invalid
	    	 | Time.Time => raise Invalid

	end


fun close archive = bug "stub"

fun new {filename, dolFilename, dslFilenames, environment, executable} =
    A {dirty = ref false,
       filename = filename,
       manifest = Manifest.new {dolFilename = dolFilename, 
				dslFilenames = dslFilenames, 
				environment = environment, 
				executables = [executable]},
       workingPath = ""}

fun destroy archive = bug "stub"

fun findExecutable f archive =
    List.find f (Manifest.executables (manifest archive))


end
