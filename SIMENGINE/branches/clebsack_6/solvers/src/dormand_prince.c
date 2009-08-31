// Dormand-Prince (ode45) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"
#include "stdio.h"

dormand_prince_mem *SOLVER(dormand_prince, init, TARGET, SIMENGINE_STORAGE, solver_props *props) {
  int i;
#if defined TARGET_GPU
  GPU_ENTRY(init, SIMENGINE_STORAGE);

  // Temporary CPU copies of GPU datastructures
  dormand_prince_mem tmem;
  // GPU datastructures
  dormand_prince_mem *dmem;

  CDATAFORMAT *temp_cur_timestep;
  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(dormand_prince_mem)));
  tmem.props = GPU_ENTRY(init_props, SIMENGINE_STORAGE, props);;
  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k2, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k3, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k4, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k5, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k6, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k7, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.next_states, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.z_next_states, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.cur_timestep, props->num_models*sizeof(CDATAFORMAT)));

  // Create a local copy of the initial timestep and initialize
  temp_cur_timestep = (CDATAFORMAT*)malloc(props->num_models*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    temp_cur_timestep[i] = props->timestep;

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(dormand_prince_mem), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tmem.cur_timestep, temp_cur_timestep, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));

  // Free temporary
  free(temp_cur_timestep);

  return dmem;
  
#else // Used for CPU and OPENMP targets

  dormand_prince_mem *mem = (dormand_prince_mem*)malloc(sizeof(dormand_prince_mem));

  mem->props = props;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k2 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k3 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k4 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k5 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k6 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k7 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->temp = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->next_states = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->z_next_states = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  // Allocate and initialize timesteps
  mem->cur_timestep = (CDATAFORMAT*)malloc(props->num_models*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    mem->cur_timestep[i] = props->timestep;

  return mem;
#endif
}

__DEVICE__ int SOLVER(dormand_prince, eval, TARGET, SIMENGINE_STORAGE, dormand_prince_mem *mem, unsigned int modelid) {
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
      mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(mem->cur_timestep[modelid]/5.0)*mem->k1[STATE_IDX];
    }
    ret |= model_flows(mem->props->time[modelid]+(mem->cur_timestep[modelid]/5.0), mem->temp, mem->k2, mem->props->inputs, mem->props->outputs, 0, modelid);

    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(3.0*mem->cur_timestep[modelid]/40.0)*mem->k1[STATE_IDX] +
	(9.0*mem->cur_timestep[modelid]/40.0)*mem->k2[STATE_IDX];
    }
    ret |= model_flows(mem->props->time[modelid]+(3.0*mem->cur_timestep[modelid]/10.0), mem->temp, mem->k3, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(44.0*mem->cur_timestep[modelid]/45.0)*mem->k1[STATE_IDX] +
	(-56.0*mem->cur_timestep[modelid]/15.0)*mem->k2[STATE_IDX] +
	(32.0*mem->cur_timestep[modelid]/9.0)*mem->k3[STATE_IDX];
    }
    ret |= model_flows(mem->props->time[modelid]+(4.0*mem->cur_timestep[modelid]/5.0), mem->temp, mem->k4, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(19372.0*mem->cur_timestep[modelid]/6561.0)*mem->k1[STATE_IDX] +
	(-25360.0*mem->cur_timestep[modelid]/2187.0)*mem->k2[STATE_IDX] +
	(64448.0*mem->cur_timestep[modelid]/6561.0)*mem->k3[STATE_IDX] +
	(-212.0*mem->cur_timestep[modelid]/729.0)*mem->k4[STATE_IDX];
    }
    ret |= model_flows(mem->props->time[modelid]+(8.0*mem->cur_timestep[modelid]/9.0), mem->temp, mem->k5, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(9017.0*mem->cur_timestep[modelid]/3168.0)*mem->k1[STATE_IDX] +
	(-355.0*mem->cur_timestep[modelid]/33.0)*mem->k2[STATE_IDX] +
	(46732.0*mem->cur_timestep[modelid]/5247.0)*mem->k3[STATE_IDX] +
	(49.0*mem->cur_timestep[modelid]/176.0)*mem->k4[STATE_IDX] +
	(-5103.0*mem->cur_timestep[modelid]/18656.0)*mem->k5[STATE_IDX];
    }
    ret |= model_flows(mem->props->time[modelid]+mem->cur_timestep[modelid], mem->temp, mem->k6, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->next_states[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(35.0*mem->cur_timestep[modelid]/384.0)*mem->k1[STATE_IDX] +
	(500.0*mem->cur_timestep[modelid]/1113.0)*mem->k3[STATE_IDX] +
	(125.0*mem->cur_timestep[modelid]/192.0)*mem->k4[STATE_IDX] +
	(-2187.0*mem->cur_timestep[modelid]/6784.0)*mem->k5[STATE_IDX] +
	(11.0*mem->cur_timestep[modelid]/84.0)*mem->k6[STATE_IDX];
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
      //mexPrintf("%d: k1=%g, k2=%g, k3=%g, k4=%g, k5=%g, k6=%g, k7=%g\n", i, mem->k1[STATE_IDX], mem->k2[STATE_IDX], mem->k3[STATE_IDX], mem->k4[STATE_IDX], mem->k5[STATE_IDX], mem->k6[STATE_IDX], mem->k7[STATE_IDX]);
      mem->temp[STATE_IDX] = /*next_states[STATE_IDX] + */
	mem->cur_timestep[modelid]*(E1*mem->k1[STATE_IDX] +
			       E3*mem->k3[STATE_IDX] +
			       E4*mem->k4[STATE_IDX] +
			       E5*mem->k5[STATE_IDX] +
			       E6*mem->k6[STATE_IDX] +
			       E7*mem->k7[STATE_IDX]);
      //z_next_states[STATE_IDX] = mem->props->model_states[STATE_IDX] + (71*mem->cur_timestep[modelid]/57600)*k1[STATE_IDX] + (-71*mem->cur_timestep[modelid]/16695)*k3[STATE_IDX] + (71*mem->cur_timestep[modelid]/1920)*k4[STATE_IDX] + (-17253*mem->cur_timestep[modelid]/339200)*k5[STATE_IDX] + (22*mem->cur_timestep[modelid]/525)*k6[STATE_IDX] + (-1*mem->cur_timestep[modelid]/40)*k7[STATE_IDX];
      //z_next_states[STATE_IDX] = mem->props->model_states[STATE_IDX] + (5179*mem->cur_timestep[modelid]/57600)*k1[STATE_IDX] + (7571*mem->cur_timestep[modelid]/16695)*k3[STATE_IDX] + (393*mem->cur_timestep[modelid]/640)*k4[STATE_IDX] + (-92097*mem->cur_timestep[modelid]/339200)*k5[STATE_IDX] + (187*mem->cur_timestep[modelid]/2100)*k6[STATE_IDX] + (1*mem->cur_timestep[modelid]/40)*k7[STATE_IDX];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = -1e20;
    CDATAFORMAT max_allowed_error;
    CDATAFORMAT err_sum = 0.0;
    CDATAFORMAT next_timestep;

    for(i=mem->props->statesize-1; i>=0; i--) {
      err = mem->temp[STATE_IDX];
      max_allowed_error = mem->props->reltol*MAX(fabs(mem->next_states[STATE_IDX]),fabs(mem->props->model_states[STATE_IDX]))+mem->props->abstol;


      //err = fabs(next_states[STATE_IDX]-z_next_states[STATE_IDX]);
      //max_allowed_error = RELTOL*fabs(next_states[STATE_IDX])+ABSTOL;
            //if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
			       
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio>max_error ? ratio : max_error;
      err_sum += ratio*ratio;

      //mexPrintf("%d: ratio=%g next_states=%g err=%g max_allowed_error=%g\n ", i, ratio, mem->next_states[STATE_IDX], err, max_allowed_error);
    }
    
    //CDATAFORMAT norm = max_error; 
    CDATAFORMAT norm = sqrt(err_sum/((CDATAFORMAT)mem->props->statesize));
    appropriate_step = norm <= 1;
    if (mem->cur_timestep[modelid] == min_timestep) appropriate_step = TRUE;

    if (appropriate_step)
      mem->props->time[modelid] += mem->cur_timestep[modelid];

    next_timestep = 0.9 * mem->cur_timestep[modelid]*pow(1.0/norm, 1.0/5.0);
    //fprintf(stderr,"ts: %g -> %g (norm=%g) appropriate_step=%d\n", mem->cur_timestep[modelid], next_timestep, norm, appropriate_step);
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
    mem->props->model_states[STATE_IDX] = mem->next_states[STATE_IDX];
  }
  
  return ret;
}

void SOLVER(dormand_prince, free, TARGET, SIMENGINE_STORAGE, dormand_prince_mem *mem) {
#if defined TARGET_GPU
  dormand_prince_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(dormand_prince_mem), cudaMemcpyDeviceToHost));

  GPU_ENTRY(free_props, SIMENGINE_STORAGE, tmem.props);

  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(tmem.k2));
  cutilSafeCall(cudaFree(tmem.k3));
  cutilSafeCall(cudaFree(tmem.k4));
  cutilSafeCall(cudaFree(tmem.k5));
  cutilSafeCall(cudaFree(tmem.k6));
  cutilSafeCall(cudaFree(tmem.k7));
  cutilSafeCall(cudaFree(tmem.temp));
  cutilSafeCall(cudaFree(tmem.next_states));
  cutilSafeCall(cudaFree(tmem.z_next_states));
  cutilSafeCall(cudaFree(tmem.cur_timestep));
  cutilSafeCall(cudaFree(mem));

  GPU_ENTRY(exit, SIMENGINE_STORAGE);

#else // Used for CPU and OPENMP targets

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
#endif
}
