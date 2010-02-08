
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

// Device code equivalent for memcpy()
// http://forums.nvidia.com/index.php?showtopic=97970
__host__ __device__ void *memoryCopy (void *dest, const void *src, size_t n)
{
  unsigned int i;
  char *d = (char *)dest;
  char *s = (char *)src;
  for (i = 0; i < n; i++) { d[i] = s[i]; }
  return dest;
}

#endif
