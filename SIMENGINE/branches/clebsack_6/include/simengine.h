/*** NOTE some of this is legacy of diesel compiler, may be
 * incompletely integrated. ***/
#define YES 1
#define NO 0

#define NMALLOC(NMEM, TYP) ((TYP *)MALLOC((NMEM) * sizeof(TYP)))
#define NREALLOC(PTR, NMEM, TYP) ((TYP *)REALLOC(PTR, (NMEM) * sizeof(TYP)))

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

// The type of simulation quantity values.
#ifdef SIMENGINE_DOUBLE_STORAGE
//typedef double CDATAFORMAT;
#define FLITERAL(X) X
#else
//typedef float CDATAFORMAT;
// Ensures that operations involving literal quantities are not promoted to double-precision.
#define FLITERAL(X) X##f
#endif

typedef unsigned long counter;


/*** Carl's code starts here ***/
/*
 * simengine.h
 *
 * API interface to the Simatra simEngine model execution.
 *
 * Copyright 2009 Simatra Modeling Technologies
 */

typedef struct{
  unsigned int num_inputs;
  unsigned int num_states;
  unsigned int num_outputs;
  char **input_names;
  char **state_names;
  char **output_names;
  double *default_inputs;
  double *default_states;
  long long hashcode;
} simengine_interface;

// Josh: variables that track counts should be declared as
// unsigned long. We should also be bounds-checking, especially
// quantities that track the number of samples. Most likely we'd run
// out of memory before overrunning a 32-bit counter but we could
// eventually be working with >20GB RAM.
typedef struct{
  int num_quantities;
  int num_samples;
  double *data;
} simengine_output;

typedef struct{
  void *(*malloc)(size_t);
  void *(*realloc)(void *, size_t);
  void (*free)(void *);
} simengine_alloc;

/*
 * simengine_getinterface()
 *
 * outputs:
 *          simengine_interface * - pointer to a static structure within a model that defines its interface
 */
simengine_interface *simengine_getinterface();



/*
 * simengine_runmodel()
 *
 * inputs:
 *         start_time - time to start simulation
 *         stop_time - time to stop simulation
 *         num_input_sets - dimensionality of inputs value greater than 1 indicates parallel execution of models
 *                          a value of 0 indicates the use of default inputs
 *         inputs - array of input values to the simulation (contiguous arrays when num_input_sets > 1)
 *         num_state_sets - dimensionality of states value greater than 1 indicates parallel execution of models
 *                          (num_state_sets and num_imput_sets must be equal or one value must be 1 or 0)
 *         states - array of state initial values to the simulation (contiguous arrays when num_state_sets > 1)
 *         alloc - allocation routines used for simengine_output return data
 *
 * outputs:
 *          simengine_output * - returns an array of output structures with the data produced by the models
 *                               outputs from a single model are contiguous
 */
simengine_output *simengine_runmodel(int start_time, int stop_time, int num_input_sets, double *inputs, int num_state_sets, double *states, simengine_alloc *alloc);

