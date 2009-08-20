// Bogacki-Shampine (ode23) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"

bogacki_shampine_mem *SOLVER(bogacki_shampine, init, TARGET, SIMENGINE_STORAGE, solver_props *props) {
  int i;
#if defined TARGET_GPU
  GPU_ENTRY(init, SIMENGINE_STORAGE);

  // Temporary CPU copies of GPU datastructures
  bogacki_shampine_mem tmem;
  // GPU datastructures
  bogacki_shampine_mem *dmem;

  CDATAFORMAT *temp_cur_timestep;
  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(bogacki_shampine_mem)));
  tmem.props = GPU_ENTRY(init_props, SIMENGINE_STORAGE, props);;
  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k2, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k3, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k4, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.next_states, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.z_next_states, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.cur_timestep, props->num_models*sizeof(CDATAFORMAT)));

  // Create a local copy of the initial timestep and initialize
  temp_cur_timestep = (CDATAFORMAT*)malloc(props->num_models*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    temp_cur_timestep[i] = props->timestep;

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(bogacki_shampine_mem), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tmem.cur_timestep, temp_cur_timestep, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));

  // Free temporary
  free(temp_cur_timestep);

  return dmem;
  
#else // Used for CPU and OPENMP targets

  bogacki_shampine_mem *mem = (bogacki_shampine_mem*)malloc(sizeof(bogacki_shampine_mem));

  mem->props = props;
  mem->k1 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k2 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k3 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k4 = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->temp = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->next_states = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->z_next_states = malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  // Allocate and initialize timesteps
  mem->cur_timestep = malloc(props->num_models*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    mem->cur_timestep[i] = props->timestep;

  return mem;
#endif
}

__DEVICE__ int SOLVER(bogacki_shampine, eval, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem, unsigned int modelid) {
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
	(mem->cur_timestep[modelid]/2)*mem->k1[STATE_IDX];
    }
    ret |= model_flows(mem->props->time[modelid]+(mem->cur_timestep[modelid]/2), mem->temp, mem->k2, mem->props->inputs, mem->props->outputs, 0, modelid);

    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->temp[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(3*mem->cur_timestep[modelid]/4)*mem->k2[STATE_IDX];
    }
    ret |= model_flows(mem->props->time[modelid]+(3*mem->cur_timestep[modelid]/4), mem->temp, mem->k3, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->next_states[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(2.0/9.0)*mem->cur_timestep[modelid]*mem->k1[STATE_IDX] +
	(1.0/3.0)*mem->cur_timestep[modelid]*mem->k2[STATE_IDX] +
	(4.0/9.0)*mem->cur_timestep[modelid]*mem->k3[STATE_IDX];
    }
    
    // now compute k4 to adapt the step size
    ret |= model_flows(mem->props->time[modelid]+mem->cur_timestep[modelid], mem->next_states, mem->k4, mem->props->inputs, mem->props->outputs, 0, modelid);
    
    for(i=mem->props->statesize-1; i>=0; i--) {
      mem->z_next_states[STATE_IDX] = mem->props->model_states[STATE_IDX] +
	(7.0/24.0)*mem->cur_timestep[modelid]*mem->k1[STATE_IDX] +
	0.25*mem->cur_timestep[modelid]*mem->k2[STATE_IDX] +
	(1.0/3.0)*mem->cur_timestep[modelid]*mem->k3[STATE_IDX] +
	0.125*mem->cur_timestep[modelid]*mem->k4[STATE_IDX];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = -1e20;
    CDATAFORMAT max_allowed_error;
    CDATAFORMAT err_sum = 0;
    CDATAFORMAT next_timestep;

    for(i=mem->props->statesize-1; i>=0; i--) {
      err = fabs(mem->next_states[STATE_IDX]-mem->z_next_states[STATE_IDX]);
      max_allowed_error = mem->props->reltol*fabs(mem->next_states[STATE_IDX])+mem->props->abstol;
      //if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
      
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio>max_error ? ratio : max_error;
      err_sum += ratio*ratio;
      //mexPrintf("%g (%g-%g) ", ratio, next_states[STATE_IDX], z_next_states[STATE_IDX]);

    }
    
    //CDATAFORMAT norm = max_error;
    CDATAFORMAT norm = sqrt(err_sum/mem->props->statesize);
    appropriate_step = norm <= 1;
    if (mem->cur_timestep[modelid] == min_timestep) appropriate_step = TRUE;

    if (appropriate_step)
      mem->props->time[modelid] += mem->cur_timestep[modelid];

    next_timestep = 0.90 * mem->cur_timestep[modelid]*pow(1.0/norm, 1.0/3.0);
#if defined __DEVICE_EMULATION__
    //fprintf(stderr,"model: %d ts: %g -> %g (norm=%g)\n", modelid, mem->cur_timestep[modelid], next_timestep, norm);
#endif

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

void SOLVER(bogacki_shampine, free, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem) {
#if defined TARGET_GPU
  bogacki_shampine_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(bogacki_shampine_mem), cudaMemcpyDeviceToHost));

  GPU_ENTRY(free_props, SIMENGINE_STORAGE, tmem.props);

  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(tmem.k2));
  cutilSafeCall(cudaFree(tmem.k3));
  cutilSafeCall(cudaFree(tmem.k4));
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
  free(mem->temp);
  free(mem->next_states);
  free(mem->z_next_states);
  free(mem->cur_timestep);
  free(mem);
#endif
}
