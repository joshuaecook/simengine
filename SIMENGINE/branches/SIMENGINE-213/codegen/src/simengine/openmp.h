// TARGET_OPENMP allows multiple models to be executed on the CPU and uses an array of structures to hold data (prevents false sharing in cache between threads)
#define TARGET OPENMP
#define TARGET_IDX AS_IDX
#define STRUCT_IDX modelid
#define STRUCT_SIZE PARALLEL_MODELS
#define ARRAY_IDX 0
#define ARRAY_SIZE 1
#ifndef VECTOR_WIDTH
#define VECTOR_WIDTH 1
#endif
#define CPU_BLOCK_SIZE 1
#define BLOCK_WIDTH(MODELS) (CPU_BLOCK_SIZE < MODELS ? CPU_BLOCK_SIZE : MODELS)
#define GRID_WIDTH(MODELS) ((MODELS + CPU_BLOCK_SIZE - 1) / CPU_BLOCK_SIZE)
#define VECTOR_IDX(threadid) (threadid & (VECTOR_WIDTH-1))
#define __DEVICE__
#define __HOST__
#define __GLOBAL__
#define __SHARED__

static const char target[] = "parallelcpu";
