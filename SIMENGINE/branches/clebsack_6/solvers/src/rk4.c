// Runga-Kutta (4th order) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"

rk4_mem *SOLVER(rk4, init, TARGET, SIMENGINE_STORAGE, solver_props *props) {
  rk4_mem *mem = (rk4_mem*)malloc(sizeof(rk4_mem));

  mem->props = props;
  mem->k1 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k2 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k3 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k4 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->temp = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return mem;
}

__DEVICE__ int SOLVER(rk4, eval, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, unsigned int modelid) {
  int i;
  int ret;
  ret = model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);
  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
      (mem->props->timestep/2)*mem->k1[STATE_IDX];
  }
  ret |= model_flows(mem->props->time[modelid]+(mem->props->timestep/2), mem->temp, mem->k2, mem->props->inputs, mem->props->outputs, 0, modelid);

  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
      (mem->props->timestep/2)*mem->k2[STATE_IDX];
  }
  ret |= model_flows(mem->props->time[modelid]+(mem->props->timestep/2), mem->temp, mem->k3, mem->props->inputs, mem->props->outputs, 0, modelid);

  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
      mem->props->timestep*mem->k3[STATE_IDX];
  }
  ret |= model_flows(mem->props->time[modelid]+mem->props->timestep, mem->temp, mem->k4, mem->props->inputs, mem->props->outputs, 0, modelid);

  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->props->model_states[STATE_IDX] = mem->props->model_states[STATE_IDX] +
      (mem->props->timestep/6.0) * (mem->k1[STATE_IDX] +
				    2*mem->k2[STATE_IDX] +
				    2*mem->k3[STATE_IDX] +
				    mem->k4[STATE_IDX]);
  }

  mem->props->time[modelid] += mem->props->timestep;

  return ret;
}

void SOLVER(rk4, free, TARGET, SIMENGINE_STORAGE, rk4_mem *mem) {
  free(mem->k1);
  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->temp);
  free(mem);
}
