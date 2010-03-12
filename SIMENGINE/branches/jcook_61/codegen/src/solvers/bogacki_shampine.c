// Bogacki-Shampine (ode23) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

typedef struct {
  CDATAFORMAT *k1;
  CDATAFORMAT *k2;
  CDATAFORMAT *k3;
  CDATAFORMAT *k4;
  CDATAFORMAT *temp;
  CDATAFORMAT *z_next_states;
  CDATAFORMAT *cur_timestep;
} bogacki_shampine_mem;

__HOST__
int bogacki_shampine_init(solver_props *props){
  int i;
#if defined TARGET_GPU
  // Temporary CPU copies of GPU datastructures
  bogacki_shampine_mem tmem;
  // GPU datastructures
  bogacki_shampine_mem *dmem;

  CDATAFORMAT *temp_cur_timestep;
  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(bogacki_shampine_mem)));
  props->mem = dmem;
  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k2, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k3, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k4, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.z_next_states, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.cur_timestep, PARALLEL_MODELS*sizeof(CDATAFORMAT)));

  // Create a local copy of the initial timestep and initialize
  temp_cur_timestep = (CDATAFORMAT*)malloc(PARALLEL_MODELS*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    temp_cur_timestep[i] = props->timestep;

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(bogacki_shampine_mem), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tmem.cur_timestep, temp_cur_timestep, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));

  // Free temporary
  free(temp_cur_timestep);

#else // Used for CPU and OPENMP targets

  bogacki_shampine_mem *mem = (bogacki_shampine_mem*)malloc(sizeof(bogacki_shampine_mem));

  props->mem = mem;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
  mem->k2 = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
  mem->k3 = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
  mem->k4 = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
  mem->temp = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
  mem->z_next_states = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));

  // Allocate and initialize timesteps
  mem->cur_timestep = (CDATAFORMAT*)malloc(PARALLEL_MODELS*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    mem->cur_timestep[i] = props->timestep;
#endif

  return 0;
}

__DEVICE__
int bogacki_shampine_eval(solver_props *props, unsigned int modelid){
  CDATAFORMAT max_timestep = props->timestep*1024;
  CDATAFORMAT min_timestep = props->timestep/1024;

  //fprintf(stderr, "ts=%g\n", mem->cur_timestep[modelid]);

  // Stop the solver if we have reached the stoptime
  props->running[modelid] = props->time[modelid] < props->stoptime;
  if(!props->running[modelid])
    return 0;

  bogacki_shampine_mem *mem = (bogacki_shampine_mem*)props->mem;

  int i;
  int ret = model_flows(props->time[modelid], props->model_states, mem->k1, props, 1, modelid);

  int appropriate_step = 0;

  CDATAFORMAT max_error;

  while(!appropriate_step) {

    //fprintf(stderr, "|-> ts=%g", mem->cur_timestep[modelid]);
    for(i=props->statesize-1; i>=0; i--) {
      mem->temp[STATE_IDX] = props->model_states[STATE_IDX] +
	(mem->cur_timestep[modelid]/2)*mem->k1[STATE_IDX];
    }
    ret |= model_flows(props->time[modelid]+(mem->cur_timestep[modelid]/2), mem->temp, mem->k2, props, 0, modelid);

    for(i=props->statesize-1; i>=0; i--) {
      mem->temp[STATE_IDX] = props->model_states[STATE_IDX] +
	(3*mem->cur_timestep[modelid]/4)*mem->k2[STATE_IDX];
    }
    ret |= model_flows(props->time[modelid]+(3*mem->cur_timestep[modelid]/4), mem->temp, mem->k3, props, 0, modelid);
    
    for(i=props->statesize-1; i>=0; i--) {
      props->next_states[STATE_IDX] = props->model_states[STATE_IDX] +
	(2.0/9.0)*mem->cur_timestep[modelid]*mem->k1[STATE_IDX] +
	(1.0/3.0)*mem->cur_timestep[modelid]*mem->k2[STATE_IDX] +
	(4.0/9.0)*mem->cur_timestep[modelid]*mem->k3[STATE_IDX];
    }
    
    // now compute k4 to adapt the step size
    ret |= model_flows(props->time[modelid]+mem->cur_timestep[modelid], props->next_states, mem->k4, props, 0, modelid);
    
    for(i=props->statesize-1; i>=0; i--) {
      mem->z_next_states[STATE_IDX] = props->model_states[STATE_IDX] +
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

    for(i=props->statesize-1; i>=0; i--) {
      err = fabs(props->next_states[STATE_IDX]-mem->z_next_states[STATE_IDX]);
      max_allowed_error = props->reltol*fabs(props->next_states[STATE_IDX])+props->abstol;
      //if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
      
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio>max_error ? ratio : max_error;
      err_sum += ratio*ratio;
      //mexPrintf("%g (%g-%g) ", ratio, next_states[STATE_IDX], z_next_states[STATE_IDX]);
    }
    
    //CDATAFORMAT norm = max_error;
    CDATAFORMAT norm = sqrt(err_sum/props->statesize);
    appropriate_step = norm <= 1;
    if (mem->cur_timestep[modelid] == min_timestep) appropriate_step = 1;

    if (appropriate_step){
      props->next_time[modelid] += mem->cur_timestep[modelid];
    }

    next_timestep = 0.90 * mem->cur_timestep[modelid]*pow(1.0/norm, 1.0/3.0);
#if defined __DEVICE_EMULATION__
    //fprintf(stderr,"model: %d ts: %g -> %g (norm=%g)\n", modelid, mem->cur_timestep[modelid], next_timestep, norm);
#endif

    // Try to hit the stoptime exactly
    if (next_timestep > props->stoptime - props->next_time[modelid])
      mem->cur_timestep[modelid] = props->stoptime - props->next_time[modelid];
    else if ((isnan(next_timestep)) || (next_timestep < min_timestep))
      mem->cur_timestep[modelid] = min_timestep;
    else if (next_timestep > max_timestep )
      mem->cur_timestep[modelid] = max_timestep;
    else
      mem->cur_timestep[modelid] = next_timestep;

  }

  // just return back the expected
  //for(i=props->statesize-1; i>=0; i--) {
  //  props->model_states[STATE_IDX] = props->next_states[STATE_IDX];
  //}
  
  return ret;
}

__HOST__
int bogacki_shampine_free(solver_props *props){
  assert(props);
#if defined TARGET_GPU
  bogacki_shampine_mem tmem;
  bogacki_shampine_mem *dmem = (bogacki_shampine_mem*)props->mem;

  cutilSafeCall(cudaMemcpy(&tmem, dmem, sizeof(bogacki_shampine_mem), cudaMemcpyDeviceToHost));

  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(tmem.k2));
  cutilSafeCall(cudaFree(tmem.k3));
  cutilSafeCall(cudaFree(tmem.k4));
  cutilSafeCall(cudaFree(tmem.temp));
  cutilSafeCall(cudaFree(tmem.z_next_states));
  cutilSafeCall(cudaFree(tmem.cur_timestep));
  cutilSafeCall(cudaFree(dmem));

#else // Used for CPU and OPENMP targets

  bogacki_shampine_mem *mem = (bogacki_shampine_mem*)props->mem;

  free(mem->k1);
  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->temp);
  free(mem->z_next_states);
  free(mem->cur_timestep);
  free(mem);

#endif

  return 0;
}
