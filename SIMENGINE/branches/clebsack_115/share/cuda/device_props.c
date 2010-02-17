/*
 * device_props
 *
 * Copyright (c) 2010 Simatra Modeling Technologies
 *
 * Returns a sorted list of available CUDA devices with properties colon delimeted.
 *
 */

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
    fprintf(stderr, "Failed to load CUDA runtime environment from %s.\n"
	    "\tIs LD_LIBRARY_PATH environment variable set to include CUDA libraries?\n",
	    CUDART_LIBRARY_NAME);
    return DeviceProps_NoCudaRuntime;
  }

  cudaGetDeviceCount = (cudaGetDeviceCount_f)dlsym(cudaRT, "cudaGetDeviceCount");
  cudaGetDeviceProperties = (cudaGetDeviceProperties_f)dlsym(cudaRT, "cudaGetDeviceProperties");

  if(!cudaGetDeviceCount || !cudaGetDeviceProperties){
    fprintf(stderr, "Failed to load CUDA functions from %s.\n"
	    "\tThe CUDA library found is incompatible with simEngine.\n",
	    CUDART_LIBRARY_NAME);
    return DeviceProps_NoCudaRuntime;
  }
  
  if (cudaSuccess != cudaGetDeviceCount(&ndevices)){
    fprintf(stderr, "Error obtaining device count.\n\tThe CUDA library found is incompatible with simEngine.\n");
    return DeviceProps_UnknownError;
  }

  if (0 == ndevices){
    fprintf(stderr, "No suitable devices found.\n\tIs your CUDA driver installed, and have you rebooted since installation?\n");
    return DeviceProps_NoDevices;
  }

  devices = (simCudaDevice *)malloc(sizeof(simCudaDevice) * ndevices);
  
  // Retrieve the properties for all Cuda devices
  for (deviceid = 0; deviceid < ndevices; ++deviceid){
    if (cudaSuccess != cudaGetDeviceProperties(&devices[deviceid-undevices].props, deviceid)){
      fprintf(stderr, "Error obtaining properties for device %d.\n\tThe CUDA library found is incompatible with simEngine.\n", deviceid);
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
    fprintf(stderr, "Only emulation device found.\n"
	    "\tDo you have a CUDA device?\n"
	    "\tIs the CUDA driver installed?\n"
	    "\tHave you rebooted after installing the driver?\n"
	    "\tDo you have device permissions set to allow CUDA computation?\n");
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
    // Print one device per line with properties colon separated
    fprintf(stdout, "%d:%s:%zd:%zd:%d:%d:%zd:%d:%d,%d,%d:%d,%d,%d:%zd:%d:%d:%d:%zd:%d:%d:%d:%d:%d:%d\n",
	    max_gflops_dev,
	    devices[max_gflops_dev].props.name,
	    // Switch to kB to not overflow an int
	    devices[max_gflops_dev].props.totalGlobalMem>>10,
	    devices[max_gflops_dev].props.sharedMemPerBlock,
	    devices[max_gflops_dev].props.regsPerBlock,
	    devices[max_gflops_dev].props.warpSize,
	    devices[max_gflops_dev].props.memPitch,
	    devices[max_gflops_dev].props.maxThreadsPerBlock,
	    devices[max_gflops_dev].props.maxThreadsDim[0],
	    devices[max_gflops_dev].props.maxThreadsDim[1],
	    devices[max_gflops_dev].props.maxThreadsDim[2],
	    devices[max_gflops_dev].props.maxGridSize[0],
	    devices[max_gflops_dev].props.maxGridSize[1],
	    devices[max_gflops_dev].props.maxGridSize[2],
	    devices[max_gflops_dev].props.totalConstMem,
	    devices[max_gflops_dev].props.major,
	    devices[max_gflops_dev].props.minor,
	    devices[max_gflops_dev].props.clockRate,
	    devices[max_gflops_dev].props.textureAlignment,
	    devices[max_gflops_dev].props.deviceOverlap,
	    devices[max_gflops_dev].props.multiProcessorCount,
	    devices[max_gflops_dev].props.kernelExecTimeoutEnabled,
	    devices[max_gflops_dev].props.integrated,
	    devices[max_gflops_dev].props.canMapHostMemory,
	    devices[max_gflops_dev].props.computeMode
	    );
    devices[max_gflops_dev].unsorted = 0;
  }

  return DeviceProps_Success;
}
