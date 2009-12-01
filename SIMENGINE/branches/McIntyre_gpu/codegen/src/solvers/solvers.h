// Solvers Header File
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

#ifndef SOLVERS_H
#define SOLVERS_H

#ifndef NAN
#define NAN (FLITERAL(0.0)/FLITERAL(0.0))
#endif

// Assert is not available in device code.
#if defined(__DEVICE_EMULATION__) || !defined(__CUDACC__)
#include <assert.h>
#else
#define assert(x)
#endif


// Solver indexing mode for states
#define STATE_IDX TARGET_IDX(props->statesize, props->num_models, i, modelid)

// Properties data structure
// ============================================================================================================

// Pointers to GPU device memory, used only on the Host for transfers
typedef struct {
  CDATAFORMAT *time;
  CDATAFORMAT *model_states;
  void *ob;
  void *mem;
} gpu_data;

// Additional CVODE specific options
typedef struct {
  int lmm;
  int iter;
  int solv;
  int upperhalfbw;
  int lowerhalfbw;
} cvode_opts;
// CVODE solver types
#define CVODE_DENSE 0
#define CVODE_DIAG 1
#define CVODE_BAND 2

typedef void solver_mem;

// Each iterator associates with an instance of this structure.
typedef struct {
  CDATAFORMAT timestep; // dt for fixed timestep solver, first dt for variable timestep,
                        // sample period for discrete
  CDATAFORMAT abstol;
  CDATAFORMAT reltol;
  CDATAFORMAT starttime;
  CDATAFORMAT stoptime;
  // A pointer to a systemstatedata_ptr structure
  void *system_states;
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
  unsigned int statesize;
  unsigned int outputsize;
  unsigned int num_models;
  unsigned int bandsize;  // Number of bands in statesize*statesize matrix for linear solver
  void *od;
  unsigned int ob_size;
  output_buffer *ob;
  gpu_data gpu;
  cvode_opts cvode;
  int *running;
  solver_mem *mem;
} solver_props;


// Pre-declaration of model_flows, the interface between the solver and the model
__DEVICE__ int model_flows(CDATAFORMAT iterval, const CDATAFORMAT *y, CDATAFORMAT *dydt, solver_props *props, unsigned int first_iteration, unsigned int modelid);

__DEVICE__ int model_running(solver_props *props, unsigned int modelid);

__DEVICE__ void solver_writeback(solver_props *props, unsigned int modelid){
  unsigned int i;
  // Update model states to next value
  for(i=0;i<props->statesize;i++){
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
    Iterator iter = ITERATORS[i];
    if (props[iter].running[modelid]) {
      min_time = props[iter].next_time[modelid];
      break;
    }
  }

  // Finds the running iterator with the earliest min time
  for(i=0;i<NUM_ITERATORS;i++){
    Iterator iter = ITERATORS[i];
    if(props[iter].next_time[modelid] < min_time && props[iter].running[modelid]){
      min_time = props[iter].next_time[modelid];
    }
  }

  return min_time;
}

// Check to see if any of the iterators are not yet completed
__DEVICE__ int model_running(solver_props *props, unsigned int modelid){
  unsigned int i;
  assert(NUM_ITERATORS);
  for(i=0;i<NUM_ITERATORS;i++){
    Iterator iter = ITERATORS[i];
    if(props[iter].running[modelid])
      return 1;
  }
  return 0;
}


int immediate_init(solver_props *props) {
  return 0;
}

__DEVICE__ int immediate_eval(solver_props *props, unsigned int modelid) {
  props->running[modelid] = props->time[modelid] < props->stoptime;
  if (!props->running[modelid])
    return 0;

  props->next_time[modelid] = props->stoptime;

  return model_flows(props->time[modelid], props->model_states, props->next_states, props, 1, modelid);
}

int immediate_free(solver_props *props) {
  return 0;
}


#endif // SOLVERS_H
