/* simex.h
 * Interface to the compilation and execution of Diesel models.
 *
 * Copyright (c) 2010 Simatra Modeling Technologies
 */

#ifndef SIMEX_H
#define SIMEX_H

// Standard includes
#include <stdio.h>    // snprintf(), printf(), fprintf(), stderr, stdout
#include <limits.h>   // PATH_MAX
#include <stdlib.h>   // malloc(), realloc(), free()
#include <string.h>   // memset(), strcpy(), strcat(), strdup()
#include <dlfcn.h>    // dlopen(), dlclose(), dlsym()
#include <getopt.h>   // getopt(), struct option
#include <math.h>     // isfinite for Darwin
#include <libgen.h>   // basename(), dirname()

// Custom include
#include "simengine_api.h"
#include "memory_layout.h" // AS_IDX

// This is a fix for Darwin which does not define __finite
#ifdef isfinite
#define __finite isfinite
#endif

// Options parsed from the commandline
typedef struct{
  // Release options
  char model_filename[PATH_MAX];
  char sim_filename[PATH_MAX];   // Set by runsimEngine()
  char model_name[PATH_MAX];     // Set by runsimEngine()
  double start_time;
  double stop_time;
  int num_models;
  FILE *inputs_file;             
  FILE *states_file;
  FILE *outputs_file;
  char *inputs_filename;
  char *states_filename;
  char *outputs_filename;
  char *target;
  char *precision;
  double *inputs;
  double *states;
  int gnuplot;
  // Debugging only options
#ifdef SIMEX_DEBUG
  int debug;
  int emulate;
  int profile;
  int nocompile;
#endif
} simengine_opts;

// Extract the name of the model from the full path DSL filename and set up the sim file
void set_names(simengine_opts *opts);

// Compile the DSL model to specified target
int runsimEngine (const char *simengine_bin, simengine_opts *opts);

// Retrieve function pointers to the simengine API calls for the simulation library
simengine_api *init_simulation(simengine_opts *opts);

// Close the simulation library
void release_simulation(simengine_api *api);

#endif // #ifdef SIMEX_H
