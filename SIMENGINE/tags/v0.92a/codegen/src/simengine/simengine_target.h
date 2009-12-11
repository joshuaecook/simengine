// simengine_target.h
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

#ifndef SIMENGINE_TARGET_H
#define SIMENGINE_TARGET_H

// Common definitions
#define FALSE 0
#define TRUE 1

#define YES 1
#define NO 0

#ifndef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif
#ifndef MAX
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#endif

#ifndef NAN
#define NAN (FLITERAL(0.0)/FLITERAL(0.0))
#endif
#ifndef INFINITY
#define INFINITY (FLITERAL(1.0)/FLITERAL(0.0))
#endif

#ifdef SIMENGINE_MATLAB_CLIENT
#include <mex.h>
// The following macros invoke MATLAB API functions.
#define MALLOC mxMalloc
#define REALLOC mxRealloc
#define FREE mxFree
#define PRINTF mexPrintf
#define ERROR(ID, MESSAGE, ARG...) mexErrMsgIdAndTxt(#ID, MESSAGE, ##ARG)
#define WARN(ID, MESSAGE, ARG...) mexWarnMsgIdAndTxt(#ID, MESSAGE, ##ARG)
#else
#define MALLOC malloc
#define REALLOC realloc
#define FREE free
#define PRINTF printf
// NOTE that this macro expands to multiple statements making it
// potentially dangerous in conditionals if one is in the habit of
// omitting braces.
// TODO replace with a single-statement function call?
#define ERROR(ID, MESSAGE, ARG...) {fprintf(stderr, "ERROR (%s): " MESSAGE "\n",  #ID, ##ARG); exit(-1); }
#define WARN(ID, MESSAGE, ARG...) fprintf(stderr, "WARNING (%s): " MESSAGE "\n", #ID, ##ARG)
#endif

#define NMALLOC(NMEM, TYP) ((TYP *)MALLOC((NMEM) * sizeof(TYP)))
#define NREALLOC(PTR, NMEM, TYP) ((TYP *)REALLOC(PTR, (NMEM) * sizeof(TYP)))


// The type of simulation quantity values.
#if defined SIMENGINE_STORAGE_float
#define SIMENGINE_STORAGE float
typedef float CDATAFORMAT;
// Ensures that operations involving literal quantities are not promoted to double-precision.
#define FLITERAL(X) X##f
#else
// Default to double precision
#define SIMENGINE_STORAGE double
typedef double CDATAFORMAT;
#define FLITERAL(X) X
#endif

typedef unsigned long counter;

// Target backends
// Target backends reference memory in different layouts

// TARGET_OPENMP allows multiple models to be executed on the CPU and uses an array of structures to hold data (prevents false sharing in cache between threads)
#if defined TARGET_OPENMP
#define TARGET OPENMP
#define TARGET_IDX AS_IDX
#define STRUCT_IDX modelid
#define STRUCT_SIZE NUM_MODELS
#define ARRAY_IDX 0
#define ARRAY_SIZE 1
#define __DEVICE__
#define __HOST__
#define __GLOBAL__

// TARGET_GPU allows multiple models to be executed on the GPU and uses a structure of arrays to hold data (allows for coallescing of reads/and writes across threads)
#elif defined TARGET_GPU
#if defined (__DEVICE_EMULATION__)
#define TARGET EMUGPU
#else
#define TARGET GPU
#endif
#define TARGET_IDX SA_IDX // AS_IDX, SA_IDX or SER_IDX
#define STRUCT_IDX 0
#define STRUCT_SIZE 1
#define ARRAY_IDX modelid
#define ARRAY_SIZE NUM_MODELS
#define __DEVICE__ __device__
#define __HOST__ __host__
#define __GLOBAL__ __global__

#else
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
#endif

#ifndef EXTERN_C
#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif
#endif

// TARGET SPECIFIC INDEXING MODES
//****************************************************************************//
// Parallel Structure of Arrays indexing
#define SA_IDX(STRUCT_S, ARRAY_S, STRUCT_X, ARRAY_X) ((STRUCT_X) * (ARRAY_S) + (ARRAY_X))
// Parallel Array of Structures indexing
#define AS_IDX(STRUCT_S, ARRAY_S, STRUCT_X, ARRAY_X) ((ARRAY_X) * (STRUCT_S) + (STRUCT_X))
// Serial indexing
#define SER_IDX(STRUCT_S, ARRAY_S, STRUCT_X, ARRAY_X) ((STRUCT_X))
//****************************************************************************//

#endif // SIMENGINE_TARGET_H
