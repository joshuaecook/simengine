// Runga-Kutta (4th order) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

rk4_mem *rk4_init(solver_props *props) {
  rk4_mem *mem = (rk4_mem*)malloc(sizeof(rk4_mem));

  mem->props = props;
  mem->k1 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->k2 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->k3 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->k4 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->temp = malloc(props->statesize*sizeof(CDATAFORMAT));

  return mem;
}

int rk4_eval(rk4_mem *mem) {

  int i;
  int ret;
  ret = model_flows(mem->props->time[0], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1);
  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->temp[i] = mem->props->model_states[i] +
      (mem->props->timestep/2)*mem->k1[i];
  }
  ret |= model_flows(mem->props->time[0]+(mem->props->timestep/2), mem->temp, mem->k2, mem->props->inputs, mem->props->outputs, 0);

  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->temp[i] = mem->props->model_states[i] +
      (mem->props->timestep/2)*mem->k2[i];
  }
  ret |= model_flows(mem->props->time[0]+(mem->props->timestep/2), mem->temp, mem->k3, mem->props->inputs, mem->props->outputs, 0);

  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->temp[i] = mem->props->model_states[i] +
      mem->props->timestep*mem->k3[i];
  }
  ret |= model_flows(mem->props->time[0]+mem->props->timestep, mem->temp, mem->k4, mem->props->inputs, mem->props->outputs, 0);

  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->props->model_states[i] = mem->props->model_states[i] +
      (mem->props->timestep/6.0) * (mem->k1[i] +
				    2*mem->k2[i] +
				    2*mem->k3[i] +
				    mem->k4[i]);
  }

  mem->props->time[0] += mem->props->timestep;

  return ret;
}

void rk4_free(rk4_mem *mem) {
  free(mem->k1);
  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->temp);
  free(mem);
  mem = NULL;
}
