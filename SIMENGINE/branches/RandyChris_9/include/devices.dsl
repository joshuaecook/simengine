namespace Devices
    namespace CUDA
	// Nb. Device ids use 1-based indexing via this interface.

	hidden var command = Environment.getVar("SIMENGINE") + "/bin/device_props"
	hidden function device_props ()
	    var p = Process.run(command)
	    var lines = Process.read(p)
	    Process.reap(p)
	    lines
        end

	function numDevices ()
	    [l foreach l in device_props()
  	       when l.startsWith("device ")].length()
        end

	// Returns the CUDA compute capability version for a given
	// device id, e.g. "1.1" or "1.3".
	function deviceCapability (devid)
	    var lines = device_props()
	    var search = "device " + ((devid-1).tostring()) + "\n"
	    var found = false
	    var major
	    var minor

	    foreach l in lines do
	      if found then
		  if not (isdefined major) and l.startsWith "major " then 
		      major = l.substring(7,l.length()-7) 
		  end
		  if not (isdefined minor) and l.startsWith "minor " then
		      minor = l.substring(7,l.length()-7) 
		  end
	      else
		  found = l == search
  	      end
	    end

	    if not found then
		error "Devices.CUDA.deviceCapability: invalid device id."
	    else
		major + "." + minor
	    end
        end

	// Returns the total global memory size of a given device in bytes.
	function deviceGlobalMem (devid)
	    var lines = device_props()
	    var search = "device " + ((devid-1).tostring()) + "\n"
	    var found = false
	    var totalGlobalMem

	    foreach l in lines do
	      if found then
		  if not (isdefined totalGlobalMem) and l.startsWith "totalGlobalMem " then 
		      totalGlobalMem = l.substring(15,l.length()-15) 
		  end
	      else
		  found = l == search
  	      end
	    end


	    if not found then
		error "Devices.CUDA.deviceCapability: invalid device id."
	    else
		totalGlobalMem.tonumber()
	    end
        end

	// Returns the number of multiprocessors on a given device.
	function deviceMultiprocessors (devid)
	    var lines = device_props()
	    var search = "device " + ((devid-1).tostring()) + "\n"
	    var found = false
	    var multiProcessorCount

	    foreach l in lines do
	      if found then
		  if not (isdefined multiProcessorCount) and l.startsWith "multiProcessorCount " then 
		      multiProcessorCount = l.substring(20,l.length()-20) 
		  end
	      else
		  found = l == search
  	      end
	    end


	    if not found then
		error "Devices.CUDA.deviceCapability: invalid device id."
	    else
		multiProcessorCount.tonumber()
	    end
        end
    end
end
