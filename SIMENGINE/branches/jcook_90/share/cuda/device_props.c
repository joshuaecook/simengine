#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

#include <cuda_runtime_api.h>

#ifndef CUDART_LIBRARY_NAME
#error Must define CUDART_LIBRARY_NAME (e.g. libcudart.so or libcudart.dylib)
#endif

enum status {
  DeviceProps_Success,
  DeviceProps_EmulationOnly,
  DeviceProps_NoDevices,
  DeviceProps_UnknownError,
  DeviceProps_NoCudaRuntime
};

typedef struct{
  struct cudaDeviceProp props;
  int gflops;
  int unsorted;
}simCudaDevice;

// Function pointer types to dynamically loaded functions from libcudart
typedef cudaError_t (*cudaGetDeviceCount_f)(int *);
typedef cudaError_t (*cudaGetDeviceProperties_f)(struct cudaDeviceProp*, int);

int main(int argc, char **argv){
  // Cuda Runtime interface
  void *cudaRT = NULL;
  cudaGetDeviceCount_f cudaGetDeviceCount = NULL;
  cudaGetDeviceProperties_f cudaGetDeviceProperties = NULL;

  cudaError_t cuErr;
  int ndevices; // Number of devices reported by Cuda runtime
  int undevices = 0; // Number of devices that are unusable by simEngine
  unsigned int deviceid;
  unsigned int sort;
  simCudaDevice *devices;

  cudaRT = dlopen(CUDART_LIBRARY_NAME, RTLD_NOW);
  if(!cudaRT){
    fprintf(stderr, "Failed to load Cuda runtime environment from libcudart.\n");
    return DeviceProps_NoCudaRuntime;
  }

  cudaGetDeviceCount = (cudaGetDeviceCount_f)dlsym(cudaRT, "cudaGetDeviceCount");
  cudaGetDeviceProperties = (cudaGetDeviceProperties_f)dlsym(cudaRT, "cudaGetDeviceProperties");

  if(!cudaGetDeviceCount || !cudaGetDeviceProperties){
    fprintf(stderr, "Failed to load Cuda functions from libcudart.\n");
    return DeviceProps_NoCudaRuntime;
  }
  
  if (cudaSuccess != cudaGetDeviceCount(&ndevices)){
    fprintf(stderr, "Error obtaining device count.\n");
    return DeviceProps_UnknownError;
  }

  if (0 == ndevices){
    fprintf(stderr, "No suitable devices found.\n");
    return DeviceProps_NoDevices;
  }

  devices = (simCudaDevice *)malloc(sizeof(simCudaDevice) * ndevices);
  
  // Retrieve the properties for all Cuda devices
  for (deviceid = 0; deviceid < ndevices; ++deviceid){
    if (cudaSuccess != cudaGetDeviceProperties(&devices[deviceid-undevices].props, deviceid)){
      fprintf(stderr, "Error obtaining properties for device %d.\n", deviceid);
      return DeviceProps_UnknownError;
    }
    // Filter out emulation devices
    if(9999 == devices[deviceid-undevices].props.major){
      undevices += 1;
    }
    // Track GFLOPs of real devices
    else{
      devices[deviceid-undevices].gflops = devices[deviceid-undevices].props.multiProcessorCount * devices[deviceid-undevices].props.clockRate;
      devices[deviceid-undevices].unsorted = 1;
    }
  }

  // Subtract emulation devices from device count
  ndevices -= undevices;

  if(0 == ndevices){
    fprintf(stderr, "Only emulation device found.\n");
    return DeviceProps_EmulationOnly;
  }

  // Sort the useable devices by max GFLOPs
  for(sort = 0; sort<ndevices; ++sort){
    int max_gflops = 0;
    int max_gflops_dev = 0;
    for(deviceid = 0; deviceid<ndevices; ++deviceid){
      if(devices[deviceid].unsorted && devices[deviceid].gflops > max_gflops){
	max_gflops = devices[deviceid].gflops;
	max_gflops_dev = deviceid;
      }
    }
    // Print one device per line with desired properties colon separated
    fprintf(stdout, "%d:sm_%d%d:%d:%zd\n",
	    max_gflops_dev,
	    devices[max_gflops_dev].props.major,
	    devices[max_gflops_dev].props.minor,
	    devices[max_gflops_dev].props.multiProcessorCount,
	    // have to switch to kb so it doesn't overflow an int
	    devices[max_gflops_dev].props.totalGlobalMem>>10);
    devices[max_gflops_dev].unsorted = 0;
  }

  return DeviceProps_Success;
}
