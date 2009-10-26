// Solvers Header File
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

#ifndef SOLVERS_H
#define SOLVERS_H

#include <stdlib.h>
#include <math.h>

// Solver indexing mode for states
#define STATE_IDX TARGET_IDX(props->statesize, props->num_models, i, modelid)

// Properties data structure
// ============================================================================================================

// Pointers to GPU device memory, used only on the Host for transfers
typedef struct {
  CDATAFORMAT *time;
  CDATAFORMAT *model_states;
  void *ob;
} gpu_data;

// Additional CVODE specific options
typedef struct {
  int lmm;
  int iter;
  int solv;
  void *solv_opts;
} cvode_opts;
// CVODE solver types
#define CVODE_DENSE 0
#define CVODE_DIAG 1
#define CVODE_BAND 2

typedef void solver_mem;

typedef struct {
  CDATAFORMAT timestep; // dt for fixed timestep solver, first dt for variable timestep,
                        // sample period for discrete
  CDATAFORMAT abstol;
  CDATAFORMAT reltol;
  CDATAFORMAT starttime;
  CDATAFORMAT stoptime;
  void *system_states;
  CDATAFORMAT *time;
  int *count;
  CDATAFORMAT *model_states;
  CDATAFORMAT *next_states; // Allocated/Freed by solver
  CDATAFORMAT *inputs;
  simengine_output *outputs;
  Solver solver;
  Iterator iterator;
  unsigned int inputsize;
  unsigned int statesize;
  unsigned int outputsize;
  unsigned int num_models;
  void *od;
  unsigned int ob_size;
  void *ob;
  gpu_data gpu;
  cvode_opts cvode;
  int *running;
  solver_mem *mem;
} solver_props;

// Pre-declaration of model_flows, the interface between the solver and the model
__DEVICE__ int model_flows(CDATAFORMAT iterval, const CDATAFORMAT *y, CDATAFORMAT *dydt, solver_props *props, unsigned int first_iteration, unsigned int modelid);

__DEVICE__ void solver_writeback(solver_props *props, unsigned int modelid){
  unsigned int i;
  for(i=0;i<props->statesize;i++){
    props->model_states[STATE_IDX] = props->next_states[STATE_IDX];
  }
}

__DEVICE__ Iterator find_min_t(solver_props *props, unsigned int modelid){
  Iterator iter = 0;
  Iterator i;
  CDATAFORMAT min_time = props[iter].time[modelid] * props[iter].count[modelid];

  for(i=1;i<NUM_ITERATORS;i++){
    if(props[i].time[modelid] * props[iter].count[modelid] < min_time){
      iter = i;
      min_time = props[i].time[modelid] * props[iter].count[modelid];
    }
  }

  return iter;
}

// Check to see if any of the iterators are not yet completed
__DEVICE__ int model_running(solver_props *props, unsigned int modelid){
  Iterator iter;
  for(iter=0;iter<NUM_ITERATORS;iter++){
    if(props[iter].running[modelid])
      return 1;
  }
  return 0;
}

#endif // SOLVERS_H
