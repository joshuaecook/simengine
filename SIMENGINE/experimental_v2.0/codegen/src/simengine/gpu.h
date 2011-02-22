// TARGET_GPU allows multiple models to be executed on the GPU and uses a structure of arrays to hold data (allows for coallescing of reads/and writes across threads)
#if defined (__DEVICE_EMULATION__)
#define TARGET EMUGPU
#else
#define TARGET GPU
#endif
#define TARGET_IDX SA_IDX // AS_IDX, SA_IDX or SER_IDX
#define STRUCT_IDX 0
#define STRUCT_SIZE 1
#define ARRAY_IDX modelid
#define ARRAY_SIZE PARALLEL_MODELS
#ifndef VECTOR_WIDTH
#define VECTOR_WIDTH 32
#endif
#define BLOCK_WIDTH(MODELS) ((GPU_BLOCK_SIZE) < (MODELS) ? (CPU_BLOCK_SIZE) : (MODELS))
#define GRID_WIDTH(MODELS) (((MODELS) + (GPU_BLOCK_SIZE) - 1) / (CPU_BLOCK_SIZE))
#define __DEVICE__ __device__
#define __HOST__ __host__
#define __GLOBAL__ __global__

static const char target[] = "gpu";
