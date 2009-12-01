#ifdef TARGET_GPU

// Variables in global device memory. Do not refer to these directly in user code!
// gpu_init_solver_props() returns a pointer to the global solver properties.

// Needs to be copied host-to-device. May be __constant__?
__DEVICE__ solver_props gpu_solver_props[NUM_ITERATORS];

// Needs to be copied host-to-device and device-to-host. May be __shared__?
__DEVICE__ CDATAFORMAT gpu_time[NUM_MODELS * NUM_ITERATORS];

// Does not need to be copied. May be __shared__?
__DEVICE__ CDATAFORMAT gpu_next_time[NUM_MODELS * NUM_ITERATORS];

// Needs to be copied host-to-device and device-to-host.
__DEVICE__ CDATAFORMAT gpu_model_states[NUM_MODELS * NUM_STATES];

// Does not need to be copied?
__DEVICE__ CDATAFORMAT gpu_next_states[NUM_MODELS * NUM_STATES];

// Needs to be copied host-to-device.
__DEVICE__ CDATAFORMAT gpu_inputs[NUM_MODELS * NUM_INPUTS];

// Needs to be copied device-to-host? May be __shared__?
__DEVICE__ int gpu_running[NUM_MODELS * NUM_ITERATORS];

// Needs to be copied device-to-host after each bunch of iterations.
__DEVICE__ output_buffer gpu_ob;


void gpu_init (void) {
  // FIXME Add more checking of capabilities and devices available!
  cudaSetDevice(cutGetMaxGflopsDeviceId());
}

void gpu_exit (void) {
  cudaThreadExit();
}


// Given a pointer to an array of solver properties having NUM_ITERATORS length,
// initializes a mirrored set of properties in device global memory.
solver_props *gpu_init_props (solver_props *props) {
  // A temporary host duplicate of the solver properties which will be copied to device global memory.
  solver_props tmp_props[NUM_ITERATORS];
  memcpy(tmp_props, props, NUM_ITERATORS * sizeof(solver_props));

  // Reassigns pointers within the duplicate properties structures to locations in device global memory.
  unsigned int states_offset = 0;
  for (unsigned int i = 0; i < NUM_ITERATORS; i++) {
    // Each iterator has its own area of memory
    tmp_props[i].time = gpu_time + (i * NUM_MODELS);
    tmp_props[i].next_time = gpu_next_time + (i * NUM_MODELS);
    tmp_props[i].running = gpu_running + (i * NUM_MODELS);

    props[i].gpu.time = tmp_props[i].time;

    // The amount of memory varies for each iterator
    tmp_props[i].model_states = gpu_model_states + states_offset;
    tmp_props[i].next_states = gpu_next_states + states_offset;

    props[i].gpu.model_states = tmp_props[i].model_states;

    states_offset += props[i].statesize;

    // Every iterator shares the same memory
    tmp_props[i].outputs = NULL; // not needed?
    tmp_props[i].inputs = gpu_inputs;

    props[i].gpu.ob = &gpu_ob;
  }

  // Copies properties to device.
  cutilSafeCall(cudaMemcpy(gpu_solver_props, tmp_props, NUM_ITERATORS * sizeof(solver_props), cudaMemcpyHostToDevice));
  // Copies start time to device.
  cutilSafeCall(cudaMemcpy(gpu_time, props->time, NUM_MODELS * NUM_ITERATORS * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  // Copies initial states to device (and to next states on device).
  cutilSafeCall(cudaMemcpy(gpu_model_states, props->model_states, NUM_MODELS * NUM_STATES * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(gpu_next_states, props->model_states, NUM_MODELS * NUM_STATES * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  // Copies inputs to device.
  cutilSafeCall(cudaMemcpy(gpu_inputs, props->inputs, NUM_MODELS * NUM_INPUTS * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));

  return gpu_solver_props;
}

// Copies final times and states back to host main memory.
void gpu_finalize_props (solver_props *props) {
  unsigned int states_offset = 0;
  for (unsigned int i = 0; i < NUM_ITERATORS; i++) {
    // Each iterator has its own area of memory
    cutilSafeCall(cudaMemcpy(props[i].time, gpu_time + (i * NUM_MODELS), 
			     NUM_MODELS * sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));


    // The amount of memory varies for each iterator
    cutilSafeCall(cudaMemcpy(props[i].time, gpu_model_states + states_offset, 
			     props[i].statesize * sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));

    states_offset += props[i].statesize;
  }
}
#endif // #ifdef TARGET_GPU
