// Forward Euler Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

forwardeuler_mem forwardeuler_init(solver_props props) {
  forwardeuler_mem mem;

  mem.props = props;
  mem.k1 = malloc(props.statesize*sizeof(CDATAFORMAT));

  return mem;
}

int forwardeuler_eval(forwardeuler_mem mem, CDATAFORMAT *model_states, CDATAFORMAT *time, CDATAFORMAT *params) {

  CDATAFORMAT *outputs;

  int ret = (*mem.props.fun)(*time, model_states, mem.k1, params, outputs, 1);

  int i;
  for(i=mem.props.statesize-1; i>=0; i--) {
    model_states[i] = model_states[i] + mem.props.timestep * mem.k1[i];
  }

  *time += mem.props.timestep;

  return ret;
}

void forwardeuler_free(forwardeuler_mem mem) {
  free(mem.k1);
}
