// Forward Euler Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

typedef struct {
  CDATAFORMAT *k1;
} forwardeuler_mem;

int forwardeuler_init(solver_props *props){
#if defined TARGET_GPU
  gpu_init();

  // Temporary CPU copies of GPU datastructures
  forwardeuler_mem tmem;
  // GPU datastructures
  forwardeuler_mem *dmem;
  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(forwardeuler_mem)));
  tmem.props = gpu_init_props(props);;
  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*props->num_models*sizeof(CDATAFORMAT)));

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(forwardeuler_mem), cudaMemcpyHostToDevice));

  return dmem;
  
#else // Used for CPU and OPENMP targets

  forwardeuler_mem *mem = (forwardeuler_mem*)malloc(sizeof(forwardeuler_mem));

  props->mem = mem;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return 0;

#endif // defined TARGET_GPU
}

int forwardeuler_eval(solver_props *props, unsigned int modelid){
  forwardeuler_mem *mem = props->mem;

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

int forwardeuler_free(solver_props *props){
#if defined TARGET_GPU
  forwardeuler_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(forwardeuler_mem), cudaMemcpyDeviceToHost));

  gpu_free_props(tmem.props);

  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(mem));

  gpu_exit();

#else // Used for CPU and OPENMP targets

  forwardeuler_mem *mem = props->mem;

  free(mem->k1);
  free(mem);

  return 0;
#endif // defined TARGET_GPU
}
