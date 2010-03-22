// Midpoint Integration Method
// Copyright 2010 Simatra Modeling Technologies, L.L.C.

typedef struct {
  CDATAFORMAT *temp;
} midpoint_mem;

__HOST__
int midpoint_init(solver_props *props){
#if defined TARGET_GPU
  // Temporary CPU copies of GPU datastructures
  midpoint_mem tmem;
  // GPU datastructures
  midpoint_mem *dmem;
  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(midpoint_mem)));
  props->mem = dmem;
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(midpoint_mem), cudaMemcpyHostToDevice));

#else // Used for CPU and OPENMP targets

  midpoint_mem *mem = (midpoint_mem*)malloc(sizeof(midpoint_mem));

  props->mem = mem;
  mem->temp = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
#endif

  return 0;
}

__DEVICE__
int midpoint_eval(solver_props *props, unsigned int modelid){
  int i;
  int ret;

  midpoint_mem *mem = (midpoint_mem*)props->mem;

  ret = model_flows(props->time[modelid], props->model_states, mem->temp, props, 1, modelid);

  for(i=props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = props->model_states[STATE_IDX] +
      (props->timestep/2)*mem->temp[STATE_IDX];
  }

  ret |= model_flows(props->time[modelid]+(props->timestep/2), mem->temp, props->next_states, props, 0, modelid);

  for(i=props->statesize-1; i>=0; i--) {
    props->next_states[STATE_IDX] = props->model_states[STATE_IDX] + (props->timestep/2) * props->next_states[STATE_IDX];
  }  

  props->next_time[modelid] += props->timestep;

  return ret;
}

__HOST__
int midpoint_free(solver_props *props){
#if defined TARGET_GPU
  midpoint_mem *dmem = (midpoint_mem*)props->mem;
  midpoint_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, dmem, sizeof(midpoint_mem), cudaMemcpyDeviceToHost));

  cutilSafeCall(cudaFree(tmem.temp));
  cutilSafeCall(cudaFree(dmem));

#else // Used for CPU and OPENMP targets

  midpoint_mem *mem =(midpoint_mem*)props->mem;

  free(mem->temp);
  free(mem);
#endif // defined TARGET_GPU  free(mem->k1);

  return 0;
}
