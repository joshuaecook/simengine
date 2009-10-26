#include <stdlib.h>
#include <stdio.h>

#include <cuda_runtime_api.h>

enum status {
    DeviceProps_UnknownError,
    DeviceProps_NoDevices    
    };

int main(int argc, char **argv)
    {
    cudaError_t cuErr;
    int ndevices;
    unsigned int deviceid;
    struct cudaDeviceProp props;

    if (cudaSuccess != cudaGetDeviceCount(&ndevices))
	{
	fprintf(stderr, "Error obtaining device count.\n");
	return DeviceProps_UnknownError;
	}

    if (0 == ndevices)
	{
	fprintf(stderr, "No suitable devices found.\n");
	return DeviceProps_NoDevices;
	}
    
    for (deviceid = 0; deviceid < ndevices; ++deviceid)
	{
	if (cudaSuccess != cudaGetDeviceProperties(&props, deviceid))
	    {
	    fprintf(stderr, "Error obtaining properties for device %d.\n", deviceid);
	    return DeviceProps_UnknownError;
	    }

	if (deviceid > 0) { fprintf(stdout, "\n"); }
	fprintf(stdout, "device %d\n", deviceid);
	fprintf(stdout, "major %d\n", props.major);
	fprintf(stdout, "minor %d\n", props.minor);
	fprintf(stdout, "totalGlobalMem %zd\n", props.totalGlobalMem/1024); // have to switch to kb so it doesn't overflow an int
	fprintf(stdout, "multiProcessorCount %d\n", props.multiProcessorCount);
	}
    }
