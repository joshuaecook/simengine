namespace Archive
    function createArchive (filename, dolFilename, dslFilenames) = LF archiveCreate (filename, dolFilename, dslFilenames)
    function openArchive (filename) = LF archiveOpen (filename)
    function close (archive) = LF archiveClose (archive)
    function destroy (archive) = LF archiveDestroy (archive)
    function version (archive) = LF archiveVersion (archive)
    function creationDate (archive) = LF archiveCreationDate (archive)
end
