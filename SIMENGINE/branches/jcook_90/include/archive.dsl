namespace Archive
    class Executable
      var target
      var cSourceFilename

      constructor (aTarget, aCSourceFilename)
        this.target = aTarget
        this.cSourceFilename = aCSourceFilename
      end
    end

    function createArchive (filename, dolFilename, dslFilenames, executable)
      LF archiveCreate (filename, dolFilename, dslFilenames, executable)
    end

    function openArchive (filename) = LF archiveOpen (filename)
    function destroy (archive) = LF archiveDestroy (archive)

    function version (archive) = LF archiveVersion (archive)
    function creationDate (archive) = LF archiveCreationDate (archive)

    function dolFilename (archive) = LF archiveDolFilename (archive)
    function dslFilenames (archive) = LF archiveDslFilenames (archive)
end
