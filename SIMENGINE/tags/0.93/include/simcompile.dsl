/* Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
 * For more information, please visit http://simatratechnologies.com/
 */
namespace SimCompile
  /* Executes a command in a shell.
   * Enables a sequence of processes connected by pipes to be
   * run in a single subprocess. */
  function shell (command: String, argv: Vector of String)
    var p = Process.run(command, argv)
    var out = Process.read(p)
    Process.reap(p)
    out
  end

  overload function shell (command: String)
    shell (command, [])
  end

  var osLower = shell("uname",["-s"])[1].rstrip("\n").translate("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz")
  var arch64 = shell("uname", ["-m"])[1].contains("64")

  class Make
    var CC = "gcc"
    var LD = "gcc"
    var CFLAGS = []
    var CPPFLAGS = []
    var LDFLAGS = []
    var LDLIBS = []
    var TARGET_ARCH = ["-m32"]

    /* Returns a tuple of (compiler, options)
     * suitable for application by Process.run(). */
    function compile (outfile: String, args)
      (CC, ["-c"] + TARGET_ARCH + ["-o", outfile] + CFLAGS + CPPFLAGS + args)
    end

    /* Returns a tuple of (linker, options)
     * suitable for application by Process.run(). */
    function link (outfile: String, args)
      (LD, TARGET_ARCH + ["-o", outfile] + LDFLAGS + args + LDLIBS)
    end
  end

  /* A target-specific Make configuration.
   * A derived class shall exist for each supported target backend. */
  class Target
    var debug = false
    var profile = false
    var precision = "double"
    var parallelModels = 1
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

      m.CPPFLAGS.push_front("-DPARALLEL_MODELS=" + (parallelModels.tostring()))

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
        m.TARGET_ARCH = ["-m64"]
      end
      if "darwin" == osLower then
        m.TARGET_ARCH = ["-arch", "i386", "-arch", "x86_64"]
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
    constructor (settings)
      super ()
      parallelModels = Devices.OPENMP.numProcessors()
      settings.parallel_models = parallelModels
    end

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
    var openccFlags = ["-v"]//, "-OPT:Olimit=99999"]

    constructor (settings)
      super ()

      // MP * warp size * 4 to keep high residency (probably needs tweaking)
      parallelModels = Devices.CUDA.getProp(1, "multiProcessorCount").tonumber() * 32 * 4
      settings.parallel_models = parallelModels

      precision = settings.precision
      emulate = settings.emulate

      var cc = shell("which", ["nvcc"])
      if cc.isempty() then 
	error "Could not find nvcc. Please ensure that it exists in your path."
      end
      nvcc = LF realpath (cc[1].rstrip("\n"))
      cudaInstallPath = Path.dir (Path.dir (nvcc))
      if Devices.CUDA.numDevices == 0 then
        error("Cannot target the GPU : " + Devices.CUDA.cudaErr)
      end
      // TODO: This check may need to be expanded as more devices/architectures appear (e.g. no devices currently of arch sm_12)
      var device_id = Devices.CUDA.getProp(1, "deviceId")
      var device_arch = Devices.CUDA.getProp(1, "arch")
      if not emulate and precision == "double" and device_arch <> "sm_13" then
        warning("CUDA device does not support double precision. Defaulting to single precision float.")
        precision = "float"
        settings.precision = "float"
      end
    end

    function setupMake (m: Make)
      m.CC = nvcc
      m.CPPFLAGS.push_back("-DTARGET_GPU")
      m.CFLAGS.push_front("-I" + cudaInstallPath + "/include")
      m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib")

      // Clean this up when moving simEngine and simex to subprocess calls for external interfaces (e.g. Matlab)
      if osLower == "darwin" then
        if arch64 then
          error("nVidia tools do not support 64bit architecture.")
        else
          m.TARGET_ARCH = ["-arch", "i386"]
          m.LD = "g++-4.2"
        end
      end

      if arch64 then
	m.LDFLAGS.push_front("-L" + cudaInstallPath + "/lib64")
      end

      // nvcc and gcc have different meanings for ARCH so set them specifically for
      // each as part of CFLAGS and LDFLAGS and remove the TARGET_ARCH value
      m.CFLAGS = m.TARGET_ARCH + m.CFLAGS
      m.LDFLAGS = m.TARGET_ARCH + m.LDFLAGS
      m.TARGET_ARCH = []

      // Wrap all gcc flags in --compiler-options when passed to nvcc
      m.CFLAGS = ["--compiler-options", join(" ", m.CFLAGS),
		  "--ptxas-options", join(" ", ptxasFlags),
		  "--opencc-options", join(" ", openccFlags)]
      
      // Currently we use only the first device returned from device_props program
      // which returns a list of available devices sorted by their GFLOPs
      var device_id = Devices.CUDA.getProp(1, "deviceId")
      var device_arch = Devices.CUDA.getProp(1, "arch")
      m.CFLAGS.push_back("-DSIMENGINE_CUDA_DEVICE=" + device_id)
      m.CFLAGS.push_front("-arch=" + device_arch)

      /* Does device debugging really work? I get strange errors from cuda-gdb.
      if debug then
        m.CFLAGS = ["-g", "-G"] + m.CFLAGS
      end
      */

      if emulate then
	m.CFLAGS.push_front("-deviceemu")
	m.CPPFLAGS.push_front("-D__DEVICE_EMULATION__")
      end

      m.LDLIBS.push_back("-lcudart")
    end
  end
end
