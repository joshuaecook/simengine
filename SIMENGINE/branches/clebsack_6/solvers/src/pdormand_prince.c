// Dormand-Prince (ode45) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "psolvers.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

dormand_prince_mem *dormand_prince_init(solver_props *props) {
  dormand_prince_mem *mem = (dormand_prince_mem*)malloc(sizeof(dormand_prince_mem));
  int i;

  mem->props = props;
  mem->k1 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k2 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k3 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k4 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k5 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k6 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k7 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->temp = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->next_states = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->z_next_states = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  // Allocate and initialize timesteps
  mem->cur_timestep = malloc(props->num_models*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    mem->cur_timestep[i] = props->timestep;

  return mem;
}

int dormand_prince_eval(dormand_prince_mem *mem, int modelid) {
  CDATAFORMAT max_timestep = mem->props->timestep*1024;
  CDATAFORMAT min_timestep = mem->props->timestep/1024;

  //fprintf(stderr, "ts=%g\n", mem->cur_timestep[modelid]);

  int i;
  int ret = model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);

  int appropriate_step = FALSE;

  CDATAFORMAT max_error;

  while(!appropriate_step) {

    //fprintf(stderr, "|-> ts=%g", mem->cur_timestep[modelid]);
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_INDEX] = mem->props->model_states[STATE_INDEX] +
	(mem->cur_timestep[modelid]/5.0)*mem->k1[STATE_INDEX];
    }
    ret |= model_flows(mem->props->time[modelid]+(mem->cur_timestep[modelid]/5.0), mem->temp, mem->k2, mem->props->inputs, mem->props->outputs, 0, modelid);

    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_INDEX] = mem->props->model_states[STATE_INDEX] +
	(3.0*mem->cur_timestep[modelid]/40.0)*mem->k1[STATE_INDEX] +
	(9.0*mem->cur_timestep[modelid]/40.0)*mem->k2[STATE_INDEX];
    }
    ret |= model_flows(mem->props->time[modelid]+(3.0*mem->cur_timestep[modelid]/10.0), mem->temp, mem->k3, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_INDEX] = mem->props->model_states[STATE_INDEX] +
	(44.0*mem->cur_timestep[modelid]/45.0)*mem->k1[STATE_INDEX] +
	(-56.0*mem->cur_timestep[modelid]/15.0)*mem->k2[STATE_INDEX] +
	(32.0*mem->cur_timestep[modelid]/9.0)*mem->k3[STATE_INDEX];
    }
    ret |= model_flows(mem->props->time[modelid]+(4.0*mem->cur_timestep[modelid]/5.0), mem->temp, mem->k4, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_INDEX] = mem->props->model_states[STATE_INDEX] +
	(19372.0*mem->cur_timestep[modelid]/6561.0)*mem->k1[STATE_INDEX] +
	(-25360.0*mem->cur_timestep[modelid]/2187.0)*mem->k2[STATE_INDEX] +
	(64448.0*mem->cur_timestep[modelid]/6561.0)*mem->k3[STATE_INDEX] +
	(-212.0*mem->cur_timestep[modelid]/729.0)*mem->k4[STATE_INDEX];
    }
    ret |= model_flows(mem->props->time[modelid]+(8.0*mem->cur_timestep[modelid]/9.0), mem->temp, mem->k5, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_INDEX] = mem->props->model_states[STATE_INDEX] +
	(9017.0*mem->cur_timestep[modelid]/3168.0)*mem->k1[STATE_INDEX] +
	(-355.0*mem->cur_timestep[modelid]/33.0)*mem->k2[STATE_INDEX] +
	(46732.0*mem->cur_timestep[modelid]/5247.0)*mem->k3[STATE_INDEX] +
	(49.0*mem->cur_timestep[modelid]/176.0)*mem->k4[STATE_INDEX] +
	(-5103.0*mem->cur_timestep[modelid]/18656.0)*mem->k5[STATE_INDEX];
    }
    ret |= model_flows(mem->props->time[modelid]+mem->cur_timestep[modelid], mem->temp, mem->k6, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->next_states[STATE_INDEX] = mem->props->model_states[STATE_INDEX] +
	(35.0*mem->cur_timestep[modelid]/384.0)*mem->k1[STATE_INDEX] +
	(500.0*mem->cur_timestep[modelid]/1113.0)*mem->k3[STATE_INDEX] +
	(125.0*mem->cur_timestep[modelid]/192.0)*mem->k4[STATE_INDEX] +
	(-2187.0*mem->cur_timestep[modelid]/6784.0)*mem->k5[STATE_INDEX] +
	(11.0*mem->cur_timestep[modelid]/84.0)*mem->k6[STATE_INDEX];
    }
    
    // now compute k4 to adapt the step size
    ret |= model_flows(mem->props->time[modelid]+mem->cur_timestep[modelid], mem->next_states, mem->k7, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    CDATAFORMAT E1 = 71.0/57600.0;
    CDATAFORMAT E3 = -71.0/16695.0;
    CDATAFORMAT E4 = 71.0/1920.0;
    CDATAFORMAT E5 = -17253.0/339200.0;
    CDATAFORMAT E6 = 22.0/525.0;
    CDATAFORMAT E7 = -1.0/40.0;
    for(i=mem->props->statesize-1; i>=0; i--) {
      //mexPrintf("%d: k1=%g, k2=%g, k3=%g, k4=%g, k5=%g, k6=%g, k7=%g\n", i, mem->k1[STATE_INDEX], mem->k2[STATE_INDEX], mem->k3[STATE_INDEX], mem->k4[STATE_INDEX], mem->k5[STATE_INDEX], mem->k6[STATE_INDEX], mem->k7[STATE_INDEX]);
      mem->temp[STATE_INDEX] = /*next_states[STATE_INDEX] + */
	mem->cur_timestep[modelid]*(E1*mem->k1[STATE_INDEX] +
			       E3*mem->k3[STATE_INDEX] +
			       E4*mem->k4[STATE_INDEX] +
			       E5*mem->k5[STATE_INDEX] +
			       E6*mem->k6[STATE_INDEX] +
			       E7*mem->k7[STATE_INDEX]);
      //z_next_states[STATE_INDEX] = mem->props->model_states[STATE_INDEX] + (71*mem->cur_timestep[modelid]/57600)*k1[STATE_INDEX] + (-71*mem->cur_timestep[modelid]/16695)*k3[STATE_INDEX] + (71*mem->cur_timestep[modelid]/1920)*k4[STATE_INDEX] + (-17253*mem->cur_timestep[modelid]/339200)*k5[STATE_INDEX] + (22*mem->cur_timestep[modelid]/525)*k6[STATE_INDEX] + (-1*mem->cur_timestep[modelid]/40)*k7[STATE_INDEX];
      //z_next_states[STATE_INDEX] = mem->props->model_states[STATE_INDEX] + (5179*mem->cur_timestep[modelid]/57600)*k1[STATE_INDEX] + (7571*mem->cur_timestep[modelid]/16695)*k3[STATE_INDEX] + (393*mem->cur_timestep[modelid]/640)*k4[STATE_INDEX] + (-92097*mem->cur_timestep[modelid]/339200)*k5[STATE_INDEX] + (187*mem->cur_timestep[modelid]/2100)*k6[STATE_INDEX] + (1*mem->cur_timestep[modelid]/40)*k7[STATE_INDEX];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = -1e20;
    CDATAFORMAT max_allowed_error;
    CDATAFORMAT err_sum = 0.0;
    CDATAFORMAT next_timestep;

    for(i=mem->props->statesize-1; i>=0; i--) {
      err = mem->temp[STATE_INDEX];
      max_allowed_error = mem->props->reltol*MAX(fabs(mem->next_states[STATE_INDEX]),fabs(mem->props->model_states[STATE_INDEX]))+mem->props->abstol;


      //err = fabs(next_states[STATE_INDEX]-z_next_states[STATE_INDEX]);
      //max_allowed_error = RELTOL*fabs(next_states[STATE_INDEX])+ABSTOL;
            //if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
			       
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio>max_error ? ratio : max_error;
      err_sum += ratio*ratio;

      //mexPrintf("%d: ratio=%g next_states=%g err=%g max_allowed_error=%g\n ", i, ratio, mem->next_states[STATE_INDEX], err, max_allowed_error);
    }
    
    //CDATAFORMAT norm = max_error; 
    CDATAFORMAT norm = sqrt(err_sum/((CDATAFORMAT)mem->props->statesize));
    appropriate_step = norm <= 1;
    if (mem->cur_timestep[modelid] == min_timestep) appropriate_step = TRUE;

    if (appropriate_step)
      mem->props->time[modelid] += mem->cur_timestep[modelid];

    next_timestep = 0.9 * mem->cur_timestep[modelid]*pow(1.0/norm, 1.0/5.0);
    //mexPrintf("ts: %g -> %g (norm=%g)\n", mem->cur_timestep[modelid], next_timestep, norm);
			  
    if ((isnan(next_timestep)) || (next_timestep < min_timestep))
      mem->cur_timestep[modelid] = min_timestep;
    else if (next_timestep > max_timestep )
      mem->cur_timestep[modelid] = max_timestep;
    else
      mem->cur_timestep[modelid] = next_timestep;
    
  }

  // just return back the expected
  for(i=mem->props->statesize-1; i>=0; i--) {
    mem->props->model_states[STATE_INDEX] = mem->next_states[STATE_INDEX];
  }
  
  return ret;
}

void dormand_prince_free(dormand_prince_mem *mem) {
  free(mem->k1);
  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->k5);
  free(mem->k6);
  free(mem->k7);
  free(mem->temp);
  free(mem->next_states);
  free(mem->z_next_states);
  free(mem->cur_timestep);
  free(mem);
  mem = NULL;
}
