// Solvers Header File
// Copyright 2009, 2010 Simatra Modeling Technologies, L.L.C.

#ifndef SOLVERS_H
#define SOLVERS_H

// Assert is not available in device code.
#if defined(__DEVICE_EMULATION__) || !defined(__CUDACC__)
#include <assert.h>
#else
#define assert(x)
#endif


// Solver indexing mode for states
#define STATE_IDX TARGET_IDX(props->statesize, PARALLEL_MODELS, i, modelid)

// Properties data structure
// ============================================================================================================

// Pointers to GPU device memory, used only on the Host for transfers
typedef struct {
  CDATAFORMAT *time;
  CDATAFORMAT *model_states;
  void *ob;
  void *mem;
} gpu_data;

typedef void solver_mem;

// This is a space container for solver specific options.  This structure should not be used directly
// as a specific solver should have its own structured layout and cast the props entry to its own
// representation.  However, this structure is what actually allocates the space and therefore it
// must be at least as large as the largest solver specific options structure.
typedef struct {
  CDATAFORMAT ignoreme[8];  // Increase the array size to accommodate larger options structures as needed
}solver_opts;

// Each iterator associates with an instance of this structure.
typedef struct {
  CDATAFORMAT timestep; // dt for fixed timestep solver, first dt for variable timestep,
                        // sample period for discrete
  CDATAFORMAT abstol;
  CDATAFORMAT reltol;
  CDATAFORMAT starttime;
  CDATAFORMAT stoptime;
  // A pointer to a systemstatedata_ptr structure
  top_systemstatedata *system_states;
  CDATAFORMAT *time; // Continuous iterators (discrete mapped to continuous)
  CDATAFORMAT *next_time;
  unsigned int *count; // Discrete iterators
  // A pointer into system_states to the states for this iterator
  CDATAFORMAT *model_states;
  CDATAFORMAT *next_states;
  // freeme is not currently used.
  //  CDATAFORMAT *freeme; // Keeps track of which buffer was dynamically allocated for states; 
  CDATAFORMAT *inputs;
  simengine_output *outputs;
  Solver solver;
  Iterator iterator;
  unsigned int inputsize;
  unsigned int statesize; // Number of states for this solver
  unsigned int algebraic_statesize; // Number of algebraic states dependent upon this solver's iterator
  unsigned int outputsize;
  unsigned int num_models;
  void *od;
  unsigned int ob_size;
  output_buffer *ob;
  gpu_data gpu;
  int *running;
  solver_mem *mem;  // Solver specific memory storage
  solver_opts opts; // Solver specific options
} solver_props;

// Pre-declaration of model_flows, the interface between the solver and the model
__DEVICE__ int model_flows(CDATAFORMAT iterval, CDATAFORMAT *y, CDATAFORMAT *dydt, solver_props *props, unsigned int first_iteration, unsigned int modelid);

__DEVICE__ int model_running(solver_props *props, unsigned int modelid);

__DEVICE__ void solver_writeback(solver_props *props, unsigned int modelid){
  unsigned int i;
  // Update model states to next value
  for(i=0;i<(props->statesize+props->algebraic_statesize);i++){
    props->model_states[STATE_IDX] = props->next_states[STATE_IDX];
  }

  // Update solver time to next value
  props->time[modelid] = props->next_time[modelid];

  // Only discrete iterators have a count field
  if(props->count){
    props->count[modelid]++;
  }
}

__DEVICE__ CDATAFORMAT find_min_time(solver_props *props, unsigned int modelid){
  unsigned int i;
  CDATAFORMAT min_time;

  assert(model_running(props,modelid));
  assert(NUM_ITERATORS);

  // Finds the first running iterator for the initial min time
  for(i=0;i<NUM_ITERATORS;i++) {
    if (props[i].running[modelid]) {
      min_time = props[i].next_time[modelid];
      break;
    }
  }

  // Finds the running iterator with the earliest min time
  for(i=0;i<NUM_ITERATORS;i++){
    if(props[i].next_time[modelid] < min_time && props[i].running[modelid]){
      min_time = props[i].next_time[modelid];
    }
  }

  return min_time;
}

// Check to see if any of the iterators are not yet completed
__DEVICE__ int model_running(solver_props *props, unsigned int modelid){
  unsigned int i;
  assert(NUM_ITERATORS);
  for(i=0;i<NUM_ITERATORS;i++){
    if(props[i].running[modelid])
      return 1;
  }
  return 0;
}

#endif // SOLVERS_H
