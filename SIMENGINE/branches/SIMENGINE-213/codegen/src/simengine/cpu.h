// TARGET_CPU allows multiple models to be executed serially and uses an array of structures to hold data
// Default to CPU target if not specified
#define TARGET CPU
#define TARGET_IDX AS_IDX
#define STRUCT_IDX modelid
#define STRUCT_SIZE PARALLEL_MODELS
#define ARRAY_IDX 0
#define ARRAY_SIZE 1
#ifndef VECTOR_WIDTH
#define VECTOR_WIDTH 1
#endif
#define VECTOR_IDX(threadid) (threadid & (VECTOR_WIDTH-1))
#define __DEVICE__
#define __HOST__
#define __GLOBAL__
#define __SHARED__

static const char target[] = "cpu";
