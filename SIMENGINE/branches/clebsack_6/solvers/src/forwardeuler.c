// Forward Euler Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"

forwardeuler_mem *SOLVER(forwardeuler, init, TARGET, SIMENGINE_STORAGE, solver_props *props) {
  // Change to target specific allocation!
  forwardeuler_mem *mem = (forwardeuler_mem*)malloc(sizeof(forwardeuler_mem));

  mem->props = props;
  mem->k1 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return mem;
}

__DEVICE__ int SOLVER(forwardeuler, eval, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem, unsigned int modelid) {

  int ret = model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);

  int i;
  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->props->model_states[STATE_IDX] = mem->props->model_states[STATE_IDX] +
      mem->props->timestep * mem->k1[STATE_IDX];
  }

  mem->props->time[modelid] += mem->props->timestep;

  return ret;
}

void SOLVER(forwardeuler, free, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem) {
  // Change to target specific deallocation!
  free(mem->k1);
  free(mem);
}
