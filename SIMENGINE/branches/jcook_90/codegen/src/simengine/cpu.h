// TARGET_CPU allows multiple models to be executed serially and uses an array of structures to hold data
// Default to CPU target if not specified
#define TARGET CPU
#define TARGET_IDX AS_IDX
#define STRUCT_IDX modelid
#define STRUCT_SIZE NUM_MODELS
#define ARRAY_IDX 0
#define ARRAY_SIZE 1
#define __DEVICE__
#define __HOST__
#define __GLOBAL__

static const char target[] = "cpu";
