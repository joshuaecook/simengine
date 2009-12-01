// Forward Euler Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

typedef struct {
  CDATAFORMAT *k1;
} forwardeuler_mem;

__HOST__
int forwardeuler_init(solver_props *props){
#if defined TARGET_GPU
  // Temporary CPU copies of GPU datastructures in host main memory
  forwardeuler_mem tmp_mem;
  // GPU persistent data in global memory
  forwardeuler_mem *g_mem;

  // Allocates GPU global memory for solver's persistent data
  cutilSafeCall(cudaMalloc((void **)&tmp_mem.k1, sizeof(CDATAFORMAT) * props->statesize * props->num_models));

  // Copies solver's persistent data structure to device memory
  cutilSafeCall(cudaMalloc((void **)&g_mem, sizeof(forwardeuler_mem)));
  cutilSafeCall(cudaMemcpy(g_mem, &tmp_mem, sizeof(forwardeuler_mem), cudaMemcpyHostToDevice));

  props->mem = g_mem;

  return 0;
  
#else // Used for CPU and OPENMP targets

  forwardeuler_mem *mem = (forwardeuler_mem*)malloc(sizeof(forwardeuler_mem));

  props->mem = mem;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return 0;

#endif // defined TARGET_GPU
}

__DEVICE__
int forwardeuler_eval(solver_props *props, unsigned int modelid){
  forwardeuler_mem *mem = (forwardeuler_mem *)props->mem;

  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  props->running[modelid] = props->time[modelid] + props->timestep <= props->stoptime;
  if(!props->running[modelid])
    return 0;

  int ret = model_flows(props->time[modelid], props->model_states, mem->k1, props, 1, modelid);

  int i;
  for(i=props->statesize-1; i>=0; i--) {
    // Store the next state internally until updated before next iteration
    props->next_states[STATE_IDX] = props->model_states[STATE_IDX] +
      props->timestep * mem->k1[STATE_IDX];
  }

  props->next_time[modelid] += props->timestep;

  return ret;
}

__HOST__
int forwardeuler_free(solver_props *props){
#if defined TARGET_GPU
  forwardeuler_mem tmp_mem;
  forwardeuler_mem *g_mem = (forwardeuler_mem *)props->mem;

  cutilSafeCall(cudaMemcpy(&tmp_mem, g_mem, sizeof(forwardeuler_mem), cudaMemcpyDeviceToHost));

  cutilSafeCall(cudaFree(tmp_mem.k1));
  cutilSafeCall(cudaFree(g_mem));

#else // Used for CPU and OPENMP targets

  forwardeuler_mem *mem = props->mem;

  free(mem->k1);
  free(mem);
#endif // defined TARGET_GPU

  return 0;
}
