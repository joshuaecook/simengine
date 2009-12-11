
#ifdef TARGET_GPU
// Wrapper to debug failing GPU API calls
//****************************************************************************//
//  These routines were copied and modified from the nVidia Cuda SDK cutil_inline_runtime.h
//  This was modified to have a return value instead of calling exit() which will close Matlab when running simEngine as a plugin to Matlab.

#include<stdio.h>
#include<cuda_runtime_api.h>

#define cutilSafeCall(err) __cudaSafeCall(err, __FILE__, __LINE__)

inline int __cudaSafeCall( cudaError err, const char *file, const int line )
{
    if( cudaSuccess != err) {
        fprintf(stderr, "cudaSafeCall() Runtime API error in file <%s>, line %i : %s.\n",
                file, line, cudaGetErrorString( err) );
        return 1;
    }
    return 0;
}

// This function returns the best GPU (with maximum GFLOPS)
inline int cutGetMaxGflopsDeviceId()
{
	int device_count = 0;
	cudaGetDeviceCount( &device_count );

	cudaDeviceProp device_properties;
	int max_gflops_device = 0;
	int max_gflops = 0;
	
	int current_device = 0;
	cudaGetDeviceProperties( &device_properties, current_device );
	max_gflops = device_properties.multiProcessorCount * device_properties.clockRate;
	++current_device;

	while( current_device < device_count )
	{
		cudaGetDeviceProperties( &device_properties, current_device );
		int gflops = device_properties.multiProcessorCount * device_properties.clockRate;
		if( gflops > max_gflops )
		{
			max_gflops        = gflops;
			max_gflops_device = current_device;
		}
		++current_device;
	}

	return max_gflops_device;
}
#endif
