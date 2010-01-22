/*
 * simengine_api.h
 *
 * C API interface to the Simatra simEngine model execution.
 *
 * Copyright (c) 2009-2010 Simatra Modeling Technologies
 */

/*
 * simengine_getinterface()
 *
 * outputs:
 *          simengine_interface * - pointer to a static structure within a model that defines its interface
 */
/* EXTERN_C const simengine_interface *simengine_getinterface(); */

/*
 * simengine_runmodel()
 *
 * inputs:
 *         start_time - time to start simulation
 *         stop_time - time to stop simulation
 *         num_models - dimensionality of inputs value greater than 1 indicates parallel execution of models
 *         inputs - array of input values to the simulation (contiguous arrays when num_input_sets > 1)
 *         states - array of state initial values to the simulation (contiguous arrays when num_state_sets > 1)
 *         alloc - allocation routines used for simengine_output return data
 *
 * outputs:
 *          simengine_output * - returns an array of output structures with the data produced by the models
 *                               outputs from a single model are contiguous
 */
/* EXTERN_C simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states, simengine_alloc *alloc); */

/*
 * simengine_evalflow()
 *
 * Runs the model flow function once for the given time, states and inputs.
 * This will only work with models that have a single iterator, are run with only one instance 
 * (num_models = 1) and is only useful when using an external solver.
 *
 * inputs: 
 *         t      - time
 *         y      - pointer to array of state values
 *         inputs - pointer to array of input values
 *
 * outputs:
 *         dydt  - pointer to array of flow values for y states
 *
 * return:
 *         0 for success
 *         non-zero for computation error
 */

#ifndef SIMENGINE_API_H
#define SIMENGINE_API_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <stddef.h>
#include <omp.h>

// Error codes
enum{ SUCCESS,
      ERRMEM,
      ERRCOMP,
      ERRNUMMDL};

typedef struct{
  const char *name;
  const char *target;
  const char **solver_names;
  const char **iterator_names;
  const char **input_names;
  const char **state_names;
  const char **output_names;
  const double *default_inputs;
  const double *default_states;
  const unsigned int *output_num_quantities;
  const unsigned int version; // Switch this to be the return value of simengine_getinterface(&seint)?
  const unsigned int precision;
  const unsigned int num_models;
  const unsigned int num_iterators;
  const unsigned int num_inputs;
  const unsigned int num_states;
  const unsigned int num_outputs;
  const unsigned long long hashcode;
} simengine_interface;

/* Output data are stored interleaved.
 * The data of an output with Q quantities over T samples would look like
 *     [q0t0, q1t0, ... qQt0, q0t1, q1t1, ... qQt1, ... qQtT]
 */
typedef struct{
  unsigned int alloc;
  unsigned int num_quantities;
  unsigned int num_samples;
  double *data;
} simengine_output;

/* Model outputs are stored consecutively.
 * The results of M models with N outputs would look like
 *     [m0o0, m0o1, ... m0oN, m1o0, m1o1, ... m1oN, ... mMoN]
 */
typedef struct{
  unsigned int status;
  char *status_message;
  simengine_output *outputs;
  double *final_states;
  double *final_time;
} simengine_result;

// Allocation routines used for simengine_result
typedef struct{
  void *(*malloc)(size_t);
  void *(*realloc)(void *, size_t);
  void (*free)(void *);
} simengine_alloc;

// The types of the simengine API functions
typedef const simengine_interface *(*simengine_getinterface_f)(void);
typedef simengine_result *(*simengine_runmodel_f)(double, double, unsigned int, double *, double *, simengine_alloc *);
typedef int (*simengine_evalflow_f)(double, double *, double *, double *);

typedef struct {
  simengine_getinterface_f getinterface;
  simengine_runmodel_f runmodel;
  simengine_evalflow_f evalflow;
  void *driver;
} simengine_api;

// The following macros invoke MATLAB API functions.
#ifdef SIMENGINE_MATLAB_CLIENT
#include <mex.h>
#define MALLOC mxMalloc
#define REALLOC mxRealloc
#define FREE mxFree
#define PRINTF mexPrintf
#define ERROR(ID, MESSAGE, ARG...) mexErrMsgIdAndTxt(#ID, MESSAGE, ##ARG)
#define WARN(ID, MESSAGE, ARG...) mexWarnMsgIdAndTxt(#ID, MESSAGE, ##ARG)

// Functions used for C commandline interface
#else  // !SIMENGINE_MATLAB_CLIENT
#define MALLOC malloc
#define REALLOC realloc
#define FREE free
#define PRINTF printf
#define ERROR(ID, MESSAGE, ARG...) {fprintf(stderr, "ERROR (%s): " MESSAGE "\n",  #ID, ##ARG); exit(-1); }
#define WARN(ID, MESSAGE, ARG...) fprintf(stderr, "WARNING (%s): " MESSAGE "\n", #ID, ##ARG)
#endif

#define NMALLOC(NMEM, TYP) ((TYP *)MALLOC((NMEM) * sizeof(TYP)))
#define NREALLOC(PTR, NMEM, TYP) ((TYP *)REALLOC(PTR, (NMEM) * sizeof(TYP)))

// Definition of interface to externally available functions for dynamic libraries
#ifndef EXTERN_C
#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif
#endif

// Useful utility macros
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

#endif // SIMENGINE_API_H
