// Bogacki-Shampine (ode23) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bogacki_shampine_mem *bogacki_shampine_init(solver_props *props) {
  bogacki_shampine_mem *mem = (bogacki_shampine_mem*)malloc(sizeof(bogacki_shampine_mem));

  mem->props = props;
  mem->k1 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->k2 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->k3 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->k4 = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->temp = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->next_states = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->z_next_states = malloc(props->statesize*sizeof(CDATAFORMAT));
  mem->cur_timestep = props->timestep;

  return mem;
}

int bogacki_shampine_eval(bogacki_shampine_mem *mem) {
  CDATAFORMAT max_timestep = mem->props->timestep*1024;
  CDATAFORMAT min_timestep = mem->props->timestep/1024;

  //fprintf(stderr, "ts=%g\n", mem->cur_timestep);

  int i;
  int ret = model_flows(mem->props->time[0], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1);

  int appropriate_step = FALSE;

  CDATAFORMAT max_error;

  while(!appropriate_step) {

    //fprintf(stderr, "|-> ts=%g", mem->cur_timestep);
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[i] = mem->props->model_states[i] +
	(mem->cur_timestep/2)*mem->k1[i];
    }
    ret |= model_flows(mem->props->time[0]+(mem->cur_timestep/2), mem->temp, mem->k2, mem->props->inputs, mem->props->outputs, 0);

    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[i] = mem->props->model_states[i] +
	(3*mem->cur_timestep/4)*mem->k2[i];
    }
    ret |= model_flows(mem->props->time[0]+(3*mem->cur_timestep/4), mem->temp, mem->k3, mem->props->inputs, mem->props->outputs, 0);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->next_states[i] = mem->props->model_states[i] +
	(2.0/9.0)*mem->cur_timestep*mem->k1[i] +
	(1.0/3.0)*mem->cur_timestep*mem->k2[i] +
	(4.0/9.0)*mem->cur_timestep*mem->k3[i];
    }
    
    // now compute k4 to adapt the step size
    ret |= model_flows(mem->props->time[0]+mem->cur_timestep, mem->next_states, mem->k4, mem->props->inputs, mem->props->outputs, 0);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->z_next_states[i] = mem->props->model_states[i] +
	(7.0/24.0)*mem->cur_timestep*mem->k1[i] +
	0.25*mem->cur_timestep*mem->k2[i] +
	(1.0/3.0)*mem->cur_timestep*mem->k3[i] +
	0.125*mem->cur_timestep*mem->k4[i];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = -1e20;
    CDATAFORMAT max_allowed_error;
    CDATAFORMAT err_sum = 0;
    CDATAFORMAT next_timestep;

    for(i=mem->props->statesize-1; i>=0; i--) {
      err = fabs(mem->next_states[i]-mem->z_next_states[i]);
      max_allowed_error = mem->props->reltol*fabs(mem->next_states[i])+mem->props->abstol;
      //if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
      
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio>max_error ? ratio : max_error;
      err_sum += ratio*ratio;
      //mexPrintf("%g (%g-%g) ", ratio, next_states[i], z_next_states[i]);

    }
    
    //CDATAFORMAT norm = max_error;
    CDATAFORMAT norm = sqrt(err_sum/mem->props->statesize);
    appropriate_step = norm <= 1;
    if (mem->cur_timestep == min_timestep) appropriate_step = TRUE;

    if (appropriate_step)
      mem->props->time[0] += mem->cur_timestep;

    next_timestep = 0.90 * mem->cur_timestep*pow(1.0/norm, 1.0/3.0);
    //mexPrintf("ts: %g -> %g (norm=%g)\n", mem->cur_timestep, next_timestep, norm);

    if ((isnan(next_timestep)) || (next_timestep < min_timestep))
      mem->cur_timestep = min_timestep;
    else if (next_timestep > max_timestep )
      mem->cur_timestep = max_timestep;
    else
      mem->cur_timestep = next_timestep;

  }

  // just return back the expected
  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->props->model_states[i] = mem->next_states[i];
  }
  
  return ret;
}

void bogacki_shampine_free(bogacki_shampine_mem *mem) {
  free(mem->k1);
  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->temp);
  free(mem->next_states);
  free(mem->z_next_states);
  free(mem);
  mem = NULL;
}
