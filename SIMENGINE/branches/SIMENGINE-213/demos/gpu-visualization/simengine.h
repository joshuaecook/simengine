/*
 * simengine.h
 *
 * C API interface to the Simatra simEngine model execution.
 *
 * Copyright 2009 Simatra Modeling Technologies
 */

#ifndef SIMENGINE_H
#define SIMENGINE_H

#include "simengine_target.h"
#include <stddef.h>
#include <solvers.h>


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
  double *final_time;
} simengine_result;

typedef struct{
  void *(*malloc)(size_t);
  void *(*realloc)(void *, size_t);
  void (*free)(void *);
} simengine_alloc;

// The types of the simengine API functions
typedef const simengine_interface *(*simengine_getinterface_f)(void);
typedef const void *(*simengine_getoutputs_f)(void);
typedef simengine_result *(*simengine_runmodel_f)(double, double, unsigned int, double *, double *, simengine_alloc *);
typedef simengine_result *(*simengine_init_f)(unsigned int, double, CDATAFORMAT *, CDATAFORMAT, double *, CDATAFORMAT *, double *, CDATAFORMAT *, simengine_alloc *, solver_props **, void **);
typedef void (*simengine_free_solver_f)(void *, solver_props *);
typedef void (*simengine_release_result_f)(simengine_result *);
typedef int (*simengine_invoke_kernel_f)(void *, solver_props *);
typedef int (*simengine_async_invoke_kernel_f)(void *, solver_props *);
typedef int (*simengine_sync_kernel_f)(solver_props *, simengine_output *);
typedef void (*simengine_register_vertex_buffer_f)(float4 *, float4 *);
typedef void (*simengine_register_clut_f)(float4 *, unsigned int);

typedef struct {
  simengine_getinterface_f getinterface;
  simengine_getoutputs_f getoutputs;
  simengine_runmodel_f runmodel;
  simengine_init_f init;
  simengine_free_solver_f free_solver;
  simengine_release_result_f release_result;
  simengine_invoke_kernel_f invoke_kernel;
  simengine_async_invoke_kernel_f async_invoke_kernel;
  simengine_sync_kernel_f sync_kernel;
  simengine_register_vertex_buffer_f register_vertex_buffer;
  simengine_register_clut_f register_clut;
  void *driver;
} simengine_api;


#endif // SIMENGINE_H
