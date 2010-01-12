namespace Devices
    namespace CUDA
	hidden var command = Environment.getVar("SIMENGINE") + "/bin/device_props"
        hidden var proplist = []
        var cudaErr = "Device list not initialized"
        var numDevices = 0
	hidden function device_props ()
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

	    var p = Process.run(command)
            var allout = Process.readAll(p)
	    var lines = allout(1)
            var errline = join("", allout(2))
            var stat = Process.reap p
            if () == stat then
                foreach l in lines do
                    var propvals = l.strip("\n").split(":")
                    var proptable = Table.new([keyval.totuple() foreach keyval in zip(propkeys,propvals)])
                    proplist.push_back(proptable)
                end
                numDevices = proplist.length()
                cudaErr = ("Bad device ID.")
            else
                cudaErr = errline
            end
        end

        hidden var init = device_props()

        function getProp(devid, prop)
            if devid > numDevices or devid < 1 then
                error("CUDA device " + devid + " error: " + cudaErr)
            else
                if prop == "arch" then
                  "sm_" + (proplist[devid].getValue("major")) + (proplist[devid].getValue("minor"))
                else
                  proplist[devid].getValue(prop)
                end
            end
        end

    end // namespace CUDA
end
