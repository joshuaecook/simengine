#ifdef TARGET_GPU
void gpu_init (void) {
  // FIXME Add more checking of capabilities and devices available!
  cudaSetDevice(cutGetMaxGflopsDeviceId());
}

void gpu_exit (void) {
  cudaThreadExit();
}

// Takes a solver_props pointer on the CPU and returns a pointer to a mirrored structure on the GPU
solver_props *gpu_init_props (solver_props *props) {
  // Local temp
  solver_props tprops;

  // GPU datastructures
  solver_props *dprops;

  // Copy the properties to local temporary
  memcpy(&tprops, props, sizeof(solver_props));

  // Allocate GPU space for props and all pointer fields of props
  cutilSafeCall(cudaMalloc((void**)&dprops, sizeof(solver_props)));
  cutilSafeCall(cudaMalloc((void**)&tprops.time, props->num_models*sizeof(CDATAFORMAT)));
  if(props->statesize){
    cutilSafeCall(cudaMalloc((void**)&tprops.model_states, props->num_models*props->statesize*sizeof(CDATAFORMAT)));
  }
  else{
    tprops.model_states = NULL;
  }
  if(tprops.inputsize){
    cutilSafeCall(cudaMalloc((void**)&tprops.inputs, props->num_models*props->inputsize*sizeof(CDATAFORMAT)));
  }
  else{
    tprops.inputs = NULL;
  }
  cutilSafeCall(cudaMalloc((void**)&tprops.ob, props->ob_size));
  if(props->outputsize){
    cutilSafeCall(cudaMalloc((void**)&tprops.outputs, props->num_models*props->outputsize*sizeof(CDATAFORMAT)));
  }
  else{
    tprops.outputs = NULL;
  }
  cutilSafeCall(cudaMalloc((void**)&tprops.running, props->num_models*sizeof(CDATAFORMAT)));

  // Copy props to GPU
  cutilSafeCall(cudaMemcpy(dprops, &tprops, sizeof(solver_props), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tprops.time, props->time, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  if(tprops.model_states) cutilSafeCall(cudaMemcpy(tprops.model_states, props->model_states, props->num_models*props->statesize*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  if(tprops.inputs) cutilSafeCall(cudaMemcpy(tprops.inputs, props->inputs, props->num_models*props->inputsize*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tprops.ob, props->ob, props->ob_size, cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tprops.running, props->running, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));

  // Store pointers to GPU memory for data we need to be able to retrieve
  props->gpu.ob = tprops.ob;
  props->gpu.time = tprops.time;
  props->gpu.model_states = tprops.model_states;
  return dprops;
}

// Frees a GPU solver props structure
void gpu_free_props (solver_props *props) {
  solver_props tprops;

  cutilSafeCall(cudaMemcpy(&tprops, props, sizeof(solver_props), cudaMemcpyDeviceToHost));

  cutilSafeCall(cudaFree(tprops.time));
  if(tprops.model_states) cutilSafeCall(cudaFree(tprops.model_states));
  if(tprops.inputs) cutilSafeCall(cudaFree(tprops.inputs));
  cutilSafeCall(cudaFree(tprops.ob));
  if(tprops.outputs) cutilSafeCall(cudaFree(tprops.outputs));
  cutilSafeCall(cudaFree(tprops.running));
  cutilSafeCall(cudaFree(props));
}
#endif // #ifdef TARGET_GPU
