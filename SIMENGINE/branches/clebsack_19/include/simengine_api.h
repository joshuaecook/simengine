/*
 * simengine_api.h
 *
 * C API interface to the Simatra simEngine model execution.
 *
 * Copyright 2009 Simatra Modeling Technologies
 */

#ifndef SIMENGINE_API_H
#define SIMENGINE_API_H

#include <simengine_target.h>
#include <stddef.h>
#include <omp.h>

enum{ SUCCESS, ERRMEM, ERRCOMP, ERRNUMMDL};

char *simengine_errors[] = {"Success", "Out of memory error", "Flow computation error", "Wrong number of models"};

typedef struct {
  const unsigned long long hashcode; // Signature of the DSL model file
  const unsigned int num_models; // Parallel count of models given at compilation
  const char *solver; // Name of integration method
  const char *target; // Name of compute target
  const size_t precision; // Number of bytes in data storage, e.g. 4 = single-precision
  // TODO maybe include data about the compiler itself, e.g. branch id, build time, etc.
} simengine_metadata;

typedef struct{
  const unsigned long version;
  const unsigned int num_inputs;
  const unsigned int num_states;
  const unsigned int num_outputs;
  const char **input_names;
  const char **state_names;
  const char **output_names;
  const double *default_inputs;
  const double *default_states;
  const unsigned int *output_num_quantities;
  const char *name;
  const simengine_metadata *metadata;
} simengine_interface;

// Josh: variables that track counts should be declared as
// unsigned long. We should also be bounds-checking, especially
// quantities that track the number of samples. Most likely we'd run
// out of memory before overrunning a 32-bit counter but we could
// eventually be working with >20GB RAM.
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

typedef struct{
  void *(*malloc)(size_t);
  void *(*realloc)(void *, size_t);
  void (*free)(void *);
} simengine_alloc;

// The types of the simengine API functions
typedef const simengine_interface *(*simengine_getinterface_f)(void);
typedef simengine_result *(*simengine_runmodel_f)(double, double, unsigned int, double *, double *, simengine_alloc *);
typedef int (*simengine_evalflow_f)(double, double *, double *, double *);
//typedef void (*simengine_register_vertex_buffer)(float4 *);

typedef struct {
  simengine_getinterface_f getinterface;
  simengine_runmodel_f runmodel;
  simengine_evalflow_f evalflow;
  //  simengine_register_vertex_buffer register_vertex_buffer;
  void *driver;
} simengine_api;

// SHOULD MOVE THIS STRUCTURE TO A DIFFERENT LOCATION
// IT'S NOT PART OF THE EXTERNALLY ACCESSIBLE API
/* An internal data structure that maintains a buffer of output data.
 *
 * The 'count' array tracks the number of data produced for each model.
 *
 * The 'buffer' array comprises a list of tagged output data for each
 * model having the format:
 *     {tag, count, quantities[count]}
 * where 'tag' is an integer identifying a model output, 'count' is a
 * counter specifying the number of data quantities, and 'quantities'
 * is an array of actual data points.
 *
 * The 'ptr' and 'end' pointers are references to positions within 'buffer.'
 */
#define NUM_MODELS 1  // Shouldn't need once this is moved appropriately
#define BUFFER_LEN 8000
typedef struct{
  unsigned int active_models;
  unsigned int finished[NUM_MODELS];
  unsigned int full[NUM_MODELS];
  unsigned int count[NUM_MODELS];
  void *ptr[NUM_MODELS];
  void *end[NUM_MODELS];
  CDATAFORMAT buffer[BUFFER_LEN*NUM_MODELS];
} output_buffer;


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

#endif // SIMENGINE_API_H
