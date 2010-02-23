namespace Devices
    namespace CUDA
        hidden var initialized = false

        hidden var proplist = []
        var cudaErr = "Device list not initialized"
        var numDevices = 0
	hidden function device_props ()
	/* FIXME recognize and report errors */
	    var lines = LF cudaDeviceProps ()
	    cudaErr = LF cudaDevicePropsError ()

            var propkeys = ["deviceId",
			    "name",
			    "totalGlobalMem",
			    "sharedMemPerBlock",
			    "regsPerBlock",
			    "warpSize",
			    "memPitch",
			    "maxThreadsPerBlock",
			    "maxThreadsDim",
			    "maxGridSize",
			    "totalConstMem",
			    "major",
			    "minor",
			    "clockRate",
			    "textureAlignment",
			    "deviceOverlap",
			    "multiProcessorCount",
			    "kernelExecTimeoutEnabled",
			    "integrated",
			    "canMapHostMemory",
			    "computeMode"]

            foreach l in lines do
              var propvals = l.split(":")
              var proptable = Table.new([keyval.totuple() foreach keyval in zip(propkeys,propvals)])
              proplist.push_back(proptable)
            end
            numDevices = proplist.length()
	    if 0 < numDevices then
	      cudaErr = "Invalid CUDA device id"
	    end
        end

        function init ()
	  if not initialized then
	    device_props ()
	    initialized = true
	  end
	end

        function getProp(devid, prop)
	    if 0 == numDevices then
              error("CUDA device " + devid + " error: " + cudaErr)
            elseif devid > numDevices or devid < 1 then
	      error("Invalid CUDA device id")
            else
                if prop == "arch" then
                  "sm_" + (proplist[devid].getValue("major")) + (proplist[devid].getValue("minor"))
                else
                  proplist[devid].getValue(prop)
                end
            end
        end

    end // namespace CUDA

    namespace OPENMP
      function numProcessors() = LF openmpGetNumProcessors()
    end

    function init ()
      CUDA.init ()
    end
end
