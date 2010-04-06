namespace Dependency

  class ExternalDep
    var fullpath
    var architecture
    var version

    constructor (fullpath, architecture, version)
      self.fullpath = fullpath
      self.architecture = architecture
      self.version = version
    end

    function tostring ()
      fullpath + {" version " + version when version <> (), "" otherwise} + 
	         {" for architecture " + architecture when architecture <> (), "" otherwise}
    end
  end

  function parseCmdForVersion (versionRegexp, cmd, args)
    if cmd <> () then
      var p = Process.run (cmd, args)
      var result = join("\n", flatten(Process.readAll(p).tovector()))
      var v = ""

      if () == result then
        error ("Could not run " + cmd + " to get version information")
      else
        var t = getRegexpMatches(versionRegexp, result)

        if t.length() == 0 then
          error ("Could not find version information in " + cmd)
        else
          v = t[1]
        end
      end

      v
    else
      ""
    end
  end

var defaultPaths = [] //TODO: fill in

var architecture = Sys.architecture

var possibleArchitectures = ["i386","x86_64","x86-64","80386","i686","ppc64","ppc"]

var gccPath

function checkFileExistsWithPaths (name, additionalPaths: Vector of String)
  var paths =  defaultPaths + additionalPaths

  var fileexists = false
  var fullpath = "" 

  var which = Process.run("which", [name])
  var which_path = Process.readline(which)

  if which_path <> () then
    which_path = which_path.strip "\n"
  end

  var which_status = Process.reap(which)

  if which_status == 0 then
    fileexists = true
    fullpath = which_path
  else
    foreach path in paths do
      if FileSystem.isfile (Path.join (path, name)) then
        fileexists = true
        fullpath = Path.join (path, name)
      end
    end
  end

  if fileexists then 
    fullpath
  else
    if isdefined(gccPath) then
      var gcc = Process.run(gccPath, ["-print-file-name=" + name])
      var gccresult = Process.readline(gcc)

      if gccresult <> () then
        gccresult = gccresult.strip "\n"
      end
      
      if gccresult <> () and FileSystem.isfile gccresult then
        FileSystem.realpath gccresult
      else
        ()
      end
    else
      ()
    end
  end
end

function checkCmdExists (name, paths)
  checkFileExistsWithPaths (name, paths)
end

overload function checkCmdExists(name) 
  checkCmdExists (name, [])
end

var libexts = ["a", "dylib", "dll", "so"] // TODO: FIXME to use system type

function checkLibExists (name, paths)
  var path = ()
  var possiblePath
  foreach ext in libexts do
    if Environment.hasVar ("LD_LIBRARY_PATH") then
      possiblePath = checkFileExistsWithPaths (name + "." + ext, (Environment.getVar ("LD_LIBRARY_PATH").split ":") + paths)
    else
      possiblePath = checkFileExistsWithPaths (name + "." + ext, paths)
    end  

    if possiblePath <> () then
      path = possiblePath
    end
  end

  path
end

overload function checkLibExists (name)
  checkLibExists(name, [])
end

function getLibArch(fileCmd, libpath)
  var p = Process.run (fileCmd, ["-L", libpath])
  var result = Process.readline(p)
  
  var found = false

  var matchedArch = ""

  foreach arch in possibleArchitectures do
    if not found then
      if isRegexpMatch (arch, result) then
        found = true
        matchedArch = arch
      end
    end
  end
  
  matchedArch
end


  function checkDependencies ()
    var depsFailed = false

    // commands
    //ar
    //var ar_path = checkCmdExists ("ar")

    //ln
    var ln_path = checkCmdExists ("ln")

    //uname
    var uname_path = checkCmdExists ("uname")

    // commands with VERSIONS
    //gcc = "\\(GCC\\) ([0-9]+.[0-9]+.[0-9]+)" with --version
    var gcc_path = checkCmdExists ("gcc")
    if gcc_path == () then
      gcc_path = checkCmdExists("gcc-4.2")
    end

    if gcc_path <> () then
      gccPath = gcc_path
    end

    var gcc_version = parseCmdForVersion("\\(GCC\\) ([0-9]+.[0-9]+.[0-9]+)", gcc_path, ["--version"])
    if gcc_version == "" then
      gcc_version = "none"
    end

    //nvcc = "release ([0-9]+.[0-9]+)" with --version
    var nvcc_path = checkCmdExists ("nvcc", ["/usr/local/cuda/lib", "/opt/cuda/lib"])
    var nvcc_version = parseCmdForVersion ("release ([0-9]+.[0-9]+)", nvcc_path, ["--version"])

    //file = "file-([0-9]+.[0-9]+)" with --version
    var file_path = checkCmdExists ("file")
    var file_version = parseCmdForVersion ("file-([0-9]+.[0-9]+)", file_path, ["--version"])

    //make = "Make ([0-9]+.[0-9]+)" with --version
    //var make_path = checkCmdExists ("make")
    //var make_version = parseCmdForVersion ("Make ([0-9]+.[0-9]+)", make_path, ["--version"])

    //sh = "version ([0-9]+.[0-9]+.[0-9]+)" with --version
    var sh_path = checkCmdExists ("sh")
    var sh_version = parseCmdForVersion ("version ([0-9]+.[0-9]+.[0-9]+)", sh_path, ["--version"])

    // libraries
    //libdl
    var libdl_path = checkLibExists("libdl")
    var libdl_arch = getLibArch(file_path, libdl_path)

    //libz
    var libz_path = checkLibExists("libz")
    var libz_arch = getLibArch(file_path, libz_path)

    //libgomp
    var libgomp_path = checkLibExists("libgomp")
    var libgomp_arch = getLibArch(file_path, libgomp_path)

    //libcudart
    var libcudart_path = checkLibExists("libcudart", ["/usr/local/cuda/lib", "/opt/cuda/lib"])
    var libcudart_arch = getLibArch(file_path, libcudart_path)

    function dep(path, arch, version) = ExternalDep.new(path, arch, version)

    var deps = {//ar        = dep(ar_path, (), ()),
	        ln        = dep(ln_path, (), ()),
                uname     = dep(uname_path, (), ()),
                gcc       = dep(gcc_path, (), gcc_version),
                nvcc      = dep(nvcc_path, (), nvcc_version),
                file      = dep(file_path, (), file_version),
                //make      = dep(make_path, (), make_version),
                sh        = dep(sh_path, (), sh_version),
                libdl     = dep(libdl_path, libdl_arch, ()),
                libz      = dep(libz_path, libz_arch, ()),
                libgomp   = dep(libgomp_path, libgomp_arch, ()),
                libcudart = dep(libcudart_path, libcudart_arch, ())}
    
     //verify each of the above has a path
     foreach key in deps.keys do
       if (deps.getMember(key) == ()) then
         depsFailed = true
         warning ("Could not find location of " + key)
       end
     end
         
     

     //check gcc version
     if deps.gcc.version == "none" or deps.gcc.version.tonumber() < 4.1 then
       depsFailed = true
       warning ("gcc version greater than 4.1 required, " + deps.gcc.version + " found")
     end

     // check nvcc is not a symbolic link
     if nvcc_path <> FileSystem.realpath nvcc_path then
       depsFailed = true
       warning ("nvcc installation corrupted: nvcc cannot be a symbolic link")
     end

     //check architectures

     if settings.installation.depcheck.getValue() then
       test_pass(not depsFailed)
     else
       if depsFailed then
         error "Some dependencies were not satisfied"
       end
     end

     deps
  end

function buildDependencyFile ()
  var deps

  if settings.installation.depcheck.getValue() then
    notice "Checking System Dependencies"
    deps = checkDependencies()
  else
    notice "Constructing System Dependency List"

    var homedir = Environment.getVar("HOME")
    var directory = Path.join(homedir, ".simatra")
    var file = "dependencies"

    deps = checkDependencies()

    // if directory doesn't exist, create it.
    if not (FileSystem.isdir directory) then
      FileSystem.mkdir directory
    end

    // open file
    var depfile = File.openTextOut(Path.join(directory, file))  

    //foreach dependency, add a line
    foreach key in deps.keys do
      depfile.putstr(key + ":" + (deps.getValue(key).fullpath) + ":" + (deps.getValue(key).version) + ":" + (deps.getValue(key).architecture) + "\n")
    end

    // close file
    depfile.close()
  end

  deps
end

function getDependencies ()
  var homedir = Environment.getVar("HOME")
  var directory = Path.join(homedir, ".simatra")
  var file = "dependencies"

  var deps

  var depfile = Path.join(directory, file)

  if FileSystem.isfile(depfile) and not (settings.installation.depcheck.getValue()) then
    //read file into deps

    var file = File.openTextIn(depfile)
    var lines = file.getall().split "\n"
    deps = {}    

    foreach line in lines do
      if line.length() > 0 then
        var tokens = line.split ":"
        var path= tokens[2]
        var version=tokens[3]
        var arch=tokens[4]

        if path == "()" then path = () end
        if arch == "()" then arch = () end
        if version == "()" then version = () end
     
        var dep = ExternalDep.new(path, arch, version)
        deps.add(tokens[1], dep)
      end    
    end
  else
    deps = buildDependencyFile()
  end

  deps
end
  
  
end
