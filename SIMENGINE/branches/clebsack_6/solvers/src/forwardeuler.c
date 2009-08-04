// Forward Euler Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

forwardeuler_mem *forwardeuler_init(solver_props *props) {
  forwardeuler_mem *mem = (forwardeuler_mem*)malloc(sizeof(forwardeuler_mem));

  mem->props = props;
  mem->k1 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return mem;
}

int forwardeuler_eval(forwardeuler_mem *mem, int modelid) {

  int ret = model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);

  int i;
  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->props->model_states[i*mem->props->num_models + modelid] = mem->props->model_states[i*mem->props->num_models + modelid] + 
      mem->props->timestep * mem->k1[i*mem->props->num_models + modelid];
  }

  mem->props->time[modelid] += mem->props->timestep;

  return ret;
}

void forwardeuler_free(forwardeuler_mem *mem) {
  free(mem->k1);
  free(mem);
  mem = NULL;
}
