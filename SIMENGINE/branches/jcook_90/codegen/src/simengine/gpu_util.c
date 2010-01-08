
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
#endif
