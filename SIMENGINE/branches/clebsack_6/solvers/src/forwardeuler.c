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
  mem->k1 = malloc(props->statesize*sizeof(CDATAFORMAT));

  return mem;
}

int forwardeuler_eval(forwardeuler_mem *mem) {

  int ret = (mem->props->fun)(*(mem->props->time), mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1);

  int i;
  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->props->model_states[i] = mem->props->model_states[i] + mem->props->timestep * mem->k1[i];
  }

  *(mem->props->time) += mem->props->timestep;

  return ret;
}

void forwardeuler_free(forwardeuler_mem *mem) {
  free(mem->k1);
  free(mem);
  mem = NULL;
}
