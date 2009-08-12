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
#if SIMENGINE_STORAGE == double
typedef double CDATAFORMAT;
#define FLITERAL(X) X
#elif SIMENGINE_STORAGE == float
typedef float CDATAFORMAT;
// Ensures that operations involving literal quantities are not promoted to double-precision.
#define FLITERAL(X) X##f
#else
#error SIMENGINE_STORAGE not set properly (float, double)
#endif

typedef unsigned long counter;

//#define TARGET cpu // cpu, openmp, gpu

#if TARGET == cpu
#define TARGET_IDX SER_IDX
#define __DEVICE__
#elif TARGET == openmp
#define TARGET_IDX AS_IDX
#define __DEVICE__
#elif TARGET == gpu
#define TARGET_IDX SA_IDX // AS_IDX, SA_IDX or SER_IDX
#define __DEVICE__ __device__
#else
#error TARGET not properly set (cpu, openmp, gpu)
#endif

// TARGET SPECIFIC INDEXING MODES
//****************************************************************************//
// Parallel Structure of Arrays indexing
inline unsigned int SA_IDX(unsigned int struct_size, unsigned int array_size, unsigned int struct_idx, unsigned int array_idx){
  return struct_idx * array_size + array_idx;
}

// Parallel Array of Structures indexing
inline unsigned int AS_IDX(unsigned int struct_size, unsigned int array_size, unsigned int struct_idx, unsigned int array_idx){
  return array_idx * struct_size + struct_idx;
}

// Serial indexing
inline unsigned int SER_IDX(unsigned int struct_size, unsigned int array_size, unsigned int struct_idx, unsigned int array_idx){
  return struct_idx;
}
//****************************************************************************//

#endif // SIMENGINE_TARGET_H
