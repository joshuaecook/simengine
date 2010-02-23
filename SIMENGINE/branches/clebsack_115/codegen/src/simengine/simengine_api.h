/*
 * simengine_api.h
 *
 * C API interface to the Simatra simEngine model execution.
 *
 * Copyright (c) 2009-2010 Simatra Modeling Technologies
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
#include <getopt.h>

// Error codes
enum{ SUCCESS,
      ERRMEM,
      ERRCOMP};

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
  const unsigned int parallel_models;
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

// Options parsed from the commandline
typedef struct{
  double start_time;
  double stop_time;
  int num_models;
  FILE *inputs_file;             
  FILE *states_file;
  FILE *outputs_file;
  char *inputs_filename;
  char *states_filename;
  char *outputs_filename;
  double *inputs;
  double *states;
  int gnuplot;
} simengine_opts;

// Command line option parsing enumeration
typedef enum {
  NO_OPTION,
  START,
  STOP,
  INSTANCES,
  INPUT_FILE,
  STATE_INIT_FILE,
  OUTPUT_FILE,
  GNUPLOT,
  HELP
} clopts;

// Useful utility macros
#define MALLOC malloc
#define REALLOC realloc
#define FREE free
#define PRINTF printf
#define PRINTFE(ARG...) fprintf(stderr, ##ARG)
#define ERROR(ID, MESSAGE, ARG...) {fprintf(stderr, "ERROR (%s): " MESSAGE "\n",  #ID, ##ARG); exit(-1); }
#define WARN(ID, MESSAGE, ARG...) fprintf(stderr, "WARNING (%s): " MESSAGE "\n", #ID, ##ARG)

#define NMALLOC(NMEM, TYP) ((TYP *)MALLOC((NMEM) * sizeof(TYP)))
#define NREALLOC(PTR, NMEM, TYP) ((TYP *)REALLOC(PTR, (NMEM) * sizeof(TYP)))

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

// This is a fix for Darwin which does not define __finite
#ifdef __APPLE__
#define __finite isfinite
#endif

#endif // SIMENGINE_API_H
