// Runga-Kutta (4th order) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

typedef struct {
  CDATAFORMAT *k1;
  CDATAFORMAT *k2;
  CDATAFORMAT *k3;
  CDATAFORMAT *k4;
  CDATAFORMAT *temp;
} rk4_mem;

int rk4_init(solver_props *props){
#if defined TARGET_GPU
  gpu_init();

  // Temporary CPU copies of GPU datastructures
  rk4_mem tmem;
  // GPU datastructures
  rk4_mem *dmem;
  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(rk4_mem)));
  tmem.props = gpu_init_props(props);;
  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k2, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k3, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k4, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*props->num_models*sizeof(CDATAFORMAT)));

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(rk4_mem), cudaMemcpyHostToDevice));

  return dmem;
  
#else // Used for CPU and OPENMP targets

  rk4_mem *mem = (rk4_mem*)malloc(sizeof(rk4_mem));

  props->mem = mem;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k2 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k3 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k4 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->temp = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return 0;
#endif
}

int rk4_eval(solver_props *props, unsigned int modelid){
  int i;
  int ret;

  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  props->running[modelid] = props->time[modelid] + props->timestep <= props->stoptime;
  if(!props->running[modelid])
    return 0;

  rk4_mem *mem = props->mem;

  ret = model_flows(props->time[modelid], props->model_states, mem->k1, props, 1, modelid);
  for(i=props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = props->model_states[STATE_IDX] +
      (props->timestep/2)*mem->k1[STATE_IDX];
  }
  ret |= model_flows(props->time[modelid]+(props->timestep/2), mem->temp, mem->k2, props, 0, modelid);

  for(i=props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = props->model_states[STATE_IDX] +
      (props->timestep/2)*mem->k2[STATE_IDX];
  }
  ret |= model_flows(props->time[modelid]+(props->timestep/2), mem->temp, mem->k3, props, 0, modelid);

  for(i=props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = props->model_states[STATE_IDX] +
      props->timestep*mem->k3[STATE_IDX];
  }
  ret |= model_flows(props->time[modelid]+props->timestep, mem->temp, mem->k4, props, 0, modelid);

  for(i=props->statesize-1; i>=0; i--) {
    props->next_states[STATE_IDX] = props->model_states[STATE_IDX] +
      (props->timestep/6.0) * (mem->k1[STATE_IDX] +
				    2*mem->k2[STATE_IDX] +
				    2*mem->k3[STATE_IDX] +
				    mem->k4[STATE_IDX]);
  }

  props->next_time[modelid] += props->timestep;

  return ret;
}

int rk4_free(solver_props *props){
#if defined TARGET_GPU
  rk4_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(rk4_mem), cudaMemcpyDeviceToHost));

  gpu_free_props(tmem.props);

  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(tmem.k2));
  cutilSafeCall(cudaFree(tmem.k3));
  cutilSafeCall(cudaFree(tmem.k4));
  cutilSafeCall(cudaFree(tmem.temp));
  cutilSafeCall(cudaFree(mem));

  gpu_exit();

#else // Used for CPU and OPENMP targets

  rk4_mem *mem = props->mem;

  free(mem->k1);
  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->temp);
  free(mem);

  return 0;
#endif // defined TARGET_GPU  free(mem->k1);
}
