/* Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
 * For more information, please visit http://simatratechnologies.com/
 */
namespace SimCompile
  /* Executes a command in a shell.
   * Enables a sequence of processes connected by pipes to be
   * run in a single subprocess. */
  function shell (command: String)
    var p = Process.run("sh", ["-c", command])
    var out = Process.read(p)
    Process.reap(p)
    out
  end

  overload function shell (command: String, args: Vector)
    shell (command + " " + join(" ", args))
  end

  var osLower = shell("uname -s | tr [:upper:] [:lower:]")[1].rstrip("\n")
  var arch64 = not(shell("arch | grep 64").isempty())

  class Make
    var CC = "gcc"
    var LD = "gcc"
    var CFLAGS = []
    var CPPFLAGS = []
    var LDFLAGS = []
    var LDLIBS = []
    var TARGET_ARCH = "-m32"

    /* Returns a tuple of (compiler, options)
     * suitable for application by Process.run(). */
    function compile (outfile: String, args)
      (CC, ["-c", TARGET_ARCH, "-o", outfile] + CFLAGS + CPPFLAGS + args)
    end

    /* Returns a tuple of (linker, options)
     * suitable for application by Process.run(). */
    function link (outfile: String, args)
      (LD, [TARGET_ARCH, "-o", outfile] + LDFLAGS + args + LDLIBS)
    end
  end

  /* A target-specific Make configuration.
   * A derived class shall exist for each supported target backend. */
  class Target
    var num_models = 1
    var debug = false
    var profile = false
    var precision = "double"
    var cFlags = ["-W", "-Wall", "-fPIC", "-fopenmp"]
    var cppFlags = []
    var ldFlags = []
    var ldLibs = ["-ldl", "-lm", "-lgomp"]

    function make ()
      var simEngine = Environment.getVar("SIMENGINE")
      var m = Make.new()

      m.CFLAGS = cFlags.clone ()
      m.CPPFLAGS = cppFlags.clone ()
      m.LDFLAGS = ldFlags.clone ()
      m.LDLIBS = ldLibs.clone ()

      m.CPPFLAGS.push_front("-DNUM_MODELS=" + (num_models.tostring()))

      if "double" <> precision then
        m.CPPFLAGS.push_back("-DSIMENGINE_STORAGE_float")
        m.CFLAGS.push_back("-I" + simEngine + "/include/float")
      else
        m.CPPFLAGS.push_back("-DSIMENGINE_STORAGE_double")
        m.CFLAGS.push_back("-I" + simEngine + "/include/double")
      end

      m.LDFLAGS.push_back("-L" + simEngine + "/lib")

      if "darwin" == osLower then
	m.CC = "gcc-4.2"
	m.LD = "gcc-4.2"
      end

      if arch64 then
        m.TARGET_ARCH = "-m64"
      end
      if "darwin" == osLower then
        m.TARGET_ARCH = "-arch i386 -arch x86_64"
      end

      if debug then
        m.CFLAGS.push_back("-g")
        m.CFLAGS.push_back("-gdwarf-2")
      else
        m.CFLAGS.push_back("-O2")
        m.CFLAGS.push_back("-fno-strict-aliasing")
      end

      if profile then
        m.CFLAGS.push_back("-pg")
      end

      // Defers to the child class to finish the configuration.
      setupMake(m)

      m
    end

    function compile (outfile: String, args)
      var m = make ()
      m.compile(outfile, args)
    end

    function link (soname: String, outfile: String, args)
      var m = make()
      if "darwin" <> osLower then
	m.LDFLAGS.push_back("-shared")
	m.LDFLAGS.push_back("-Wl,-soname,"+soname)
      else
	m.LDFLAGS.push_back("-dynamiclib")
	m.LDFLAGS.push_back("-Wl,-install_name,"+soname)
      end
      m.link(outfile, args)
    end
  end

  class TargetCPU extends Target
    function setupMake (m: Make)
      if "double" <> precision then
	m.LDLIBS.push_back("-lcvode_float")
      else
	m.LDLIBS.push_back("-lcvode_double")
      end

      m.CPPFLAGS.push_back("-DTARGET_CPU")
    end
  end

  class TargetOpenMP extends Target
    function setupMake (m: Make)
      if "double" <> precision then
	m.LDLIBS.push_back("-lcvode_float")
      else
	m.LDLIBS.push_back("-lcvode_double")
      end

      m.CPPFLAGS.push_back("-DTARGET_OPENMP")
    end
  end

  class TargetCUDA extends Target
    var nvcc
    var emulate = false
    var cudaInstallPath
    var ptxasFlags = ["-v"]

    constructor ()
      super ()

      var cc = shell("which nvcc")
      if cc.isempty() then 
	error "Could not find nvcc. Please ensure that it exists in your path."
      end
      nvcc = LF realpath (cc[1].rstrip("\n"))
      cudaInstallPath = shell("dirname \$(dirname " + nvcc + ")")[1].rstrip("\n")
    end

    function setupMake (m: Make)
      m.CC = nvcc
      m.CPPFLAGS.push_back("-DTARGET_GPU")
      m.CFLAGS.push_front("-I" + cudaInstallPath + "/include")
      m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib")

      if "" <> arch64 then
	m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib64")
      end

      m.CFLAGS = ["--compiler-options", join(" ", m.CFLAGS),
		  "--ptxas-options", join(" ", ptxasFlags)]

      // FIXME this is not os-dependent!
      if "darwin" <> osLower then
	m.CFLAGS.push_front("-arch=sm_13")
      else
	m.CFLAGS.push_front("-arch=sm_11")
      end

      if emulate then
	m.CFLAGS.push_front("-deviceemu")
	m.CPPFLAGS.push_front("-D__DEVICE_EMULATION__")
      end

      m.LDLIBS.push_back("-lcudart")
    end
  end
end
