#ifdef TARGET_GPU
// Variables in global device memory. Do not refer to these directly in user code!
// gpu_init_props() returns a pointer to the global solver properties.

// Needs to be copied host-to-device. May be __constant__?
__DEVICE__ solver_props gpu_solver_props[NUM_ITERATORS];

// Needs to be copied host-to-device and device-to-host. May be __shared__?
__DEVICE__ CDATAFORMAT gpu_time[PARALLEL_MODELS * NUM_ITERATORS];

// Does not need to be copied. May be __shared__?
__DEVICE__ CDATAFORMAT gpu_next_time[PARALLEL_MODELS * NUM_ITERATORS];

__DEVICE__ unsigned int gpu_count[PARALLEL_MODELS * NUM_ITERATORS];

// Needs to be coped device-to-host.
__DEVICE__ top_systemstatedata gpu_system[1];

#if NUM_STATES > 0
// Needs to be copied host-to-device and device-to-host.
__DEVICE__ systemstatedata_external gpu_model_states[1];

// Does not need to be copied?
__DEVICE__ systemstatedata_external gpu_next_states[1];
#endif

__DEVICE__ unsigned int dirty_states[PARALLEL_MODELS * NUM_ITERATORS];

#if NUM_INPUTS > 0
// Needs to be copied host-to-device.
__DEVICE__ CDATAFORMAT gpu_inputs[PARALLEL_MODELS * NUM_INPUTS];
#endif

// Needs to be copied device-to-host? May be __shared__?
__DEVICE__ int gpu_running[PARALLEL_MODELS * NUM_ITERATORS];

__DEVICE__ int gpu_last_iteration[PARALLEL_MODELS * NUM_ITERATORS];

// Needs to be copied device-to-host after each bunch of iterations.
__DEVICE__ output_buffer gpu_ob[1];

#if NUM_OUTPUTS > 0
__DEVICE__ output_data gpu_od[PARALLEL_MODELS];
#endif

static int global_gpuid = -1;

__GLOBAL__ void exec_kernel_gpu(solver_props *props, int resuming, unsigned int max_iterations);

void gpu_init (void) {
#ifndef SIMENGINE_CUDA_DEVICE
#error SIMENGINE_CUDA_DEVICE not specified for a GPU simulation
#endif
  if(global_gpuid > 0){
    cutilSafeCall(cudaSetDevice(global_gpuid));
  }
  else{
    cutilSafeCall(cudaSetDevice(SIMENGINE_CUDA_DEVICE));
  }
#if CUDART_VERSION >= 3000
  cutilSafeCall(cudaFuncSetCacheConfig(exec_kernel_gpu, cudaFuncCachePreferNone));
#endif
}

void gpu_exit (void) {
  cudaThreadExit();
}

// Must be defined by generated code.
// tmp_props is an array of length NUM_ITERATORS, held in main memory on the host;
// tmp_system is a pointer to a top_systemstatedata, held in main memory on the host.
// The pointers within tmp_system reference device global memory, however.
void gpu_init_system_states_pointers (solver_props *tmp_props, top_systemstatedata *tmp_system);

void gpu_init_constants (solver_props *props);

// Given a pointer to an array of solver properties having NUM_ITERATORS length,
// initializes a mirrored set of properties in device global memory.
solver_props *gpu_init_props(solver_props *props){
  // Pointers to the statically allocated in device global memory;
  solver_props *g_props;
  top_systemstatedata *g_system;
  CDATAFORMAT *g_time, *g_next_time;
  unsigned int *g_count;
  int *g_running, *g_last_iteration;
  output_buffer *g_ob;

  // Obtains the addresses of statically allocated variables.
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_props, gpu_solver_props));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_time, gpu_time));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_next_time, gpu_next_time));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_count, gpu_count));

  if (NUM_STATES > 0 || NUM_ITERATORS > 1) {
    cutilSafeCall(cudaGetSymbolAddress((void **)&g_system, gpu_system));
  }
  else {
    g_system = NULL;
  }

# if NUM_STATES > 0
  systemstatedata_external *g_model_states, *g_next_states;
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_model_states, gpu_model_states));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_next_states, gpu_next_states));
# else
  char *g_model_states, *g_next_states;
  g_model_states = NULL;
  g_next_states = NULL;
# endif


  cutilSafeCall(cudaGetSymbolAddress((void **)&g_running, gpu_running));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_last_iteration, gpu_last_iteration));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_ob, gpu_ob));
# if NUM_OUTPUTS > 0
  output_data *g_od;
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_od, gpu_od));
# else
  char *g_od = NULL;
# endif

  // A temporary host duplicate of the solver properties which will be copied to device global memory.
  solver_props tmp_props[NUM_ITERATORS];
  memcpy(tmp_props, props, NUM_ITERATORS * sizeof(solver_props));

  // These temporary host buffers are used to translate the
  // non-contiguous host memory into a contiguous region of
  // device memory.
  CDATAFORMAT tmp_time[PARALLEL_MODELS * NUM_ITERATORS];
  int tmp_running[PARALLEL_MODELS * NUM_ITERATORS];


  // Reassigns pointers within the duplicate properties structures to locations in device global memory.
  unsigned int i, states_offset = 0;
  for (i = 0; i < NUM_ITERATORS; i++) {
    // Copies start time to temporary host buffer.
    memcpy(tmp_time + (i * PARALLEL_MODELS), props[i].time, PARALLEL_MODELS * sizeof(CDATAFORMAT));
    memcpy(tmp_running + (i * PARALLEL_MODELS), props[i].running, PARALLEL_MODELS * sizeof(int));

    // Each iterator has its own area of memory, all of the equal sizes
    tmp_props[i].time = g_time + (i * PARALLEL_MODELS);
    tmp_props[i].next_time = g_next_time + (i * PARALLEL_MODELS);
    tmp_props[i].count = g_count + (i * PARALLEL_MODELS);
    tmp_props[i].running = g_running + (i * PARALLEL_MODELS);
    tmp_props[i].last_iteration = g_last_iteration + (i * PARALLEL_MODELS);

    // The amount of memory varies for each iterator
    if (0 < props[i].statesize + props[i].algebraic_statesize) {
      tmp_props[i].model_states = ((CDATAFORMAT *)g_model_states) + (states_offset * PARALLEL_MODELS);
      tmp_props[i].next_states = ((CDATAFORMAT *)g_next_states) + (states_offset * PARALLEL_MODELS);
    }
    else {
      tmp_props[i].model_states = NULL;
      tmp_props[i].next_states = NULL;
    }

    states_offset += props[i].statesize + props[i].algebraic_statesize;

    // Every iterator shares the same memory
    tmp_props[i].system_states = g_system;
    tmp_props[i].modelid_offset = 0; // not needed
    tmp_props[i].od = g_od;

    // Pointers to device global memory that the host needs
    props[i].gpu.time = tmp_props[i].time;
    props[i].gpu.model_states = tmp_props[i].model_states;
  }

  // A temporary host duplicate of the system states pointers structure.
  top_systemstatedata tmp_system[1];
  gpu_init_system_states_pointers(tmp_props, tmp_system);

  gpu_init_constants(props);

  // Copies initial states to device (and to next states on device).
  for (i = 0; i < NUM_ITERATORS; i++) {
    // Finds the first iterator with states. 
    // Its pointer will reference the beginning of state memory.
#   if NUM_STATES > 0
    if (0 < props[i].statesize + props[i].algebraic_statesize) {
      cutilSafeCall(cudaMemcpy(g_model_states, props[i].model_states, sizeof(systemstatedata_external), cudaMemcpyHostToDevice));
      cutilSafeCall(cudaMemcpy(g_next_states, props[i].model_states, sizeof(systemstatedata_external), cudaMemcpyHostToDevice));
      break;
    }
#   endif
  }

  // Copies system states to device.
  if (NUM_STATES > 0 || NUM_ITERATORS > 1) {
    cutilSafeCall(cudaMemcpy(g_system, tmp_system, sizeof(top_systemstatedata), cudaMemcpyHostToDevice));
  }

  // Copies initial times to device.
  cutilSafeCall(cudaMemcpy(g_time, tmp_time, PARALLEL_MODELS * NUM_ITERATORS * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(g_next_time, tmp_time, PARALLEL_MODELS * NUM_ITERATORS * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));

  cutilSafeCall(cudaMemcpy(g_running, tmp_running, PARALLEL_MODELS * NUM_ITERATORS * sizeof(int), cudaMemcpyHostToDevice));

  // Copies properties to device.
  cutilSafeCall(cudaMemcpy(g_props, tmp_props, NUM_ITERATORS * sizeof(solver_props), cudaMemcpyHostToDevice));

  cutilSafeCall(cudaMemset(g_last_iteration, 0, PARALLEL_MODELS * NUM_ITERATORS * sizeof(int)));
  cutilSafeCall(cudaMemset(g_count, 0, PARALLEL_MODELS * NUM_ITERATORS * sizeof(int)));

  // Zeroes the initial output buffer to ensure the finished flags start at 0
  cutilSafeCall(cudaMemset(g_ob, 0, sizeof(output_buffer)));

  return g_props;
}

// Copies final times and states back to host main memory.
void gpu_finalize_props (solver_props *props) {
  unsigned int i;
  // A temporary host duplicate of the time vectors.
  CDATAFORMAT tmp_time[PARALLEL_MODELS * NUM_ITERATORS];

  // Copies final times from the device
  cutilSafeCall(cudaMemcpy(tmp_time, props[0].gpu.time, NUM_ITERATORS * PARALLEL_MODELS * sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
  for (i = 0; i < NUM_ITERATORS; i++) {
    // Each iterator has its own area of memory
    memcpy(props[i].time, tmp_time + (i * PARALLEL_MODELS), PARALLEL_MODELS * sizeof(CDATAFORMAT));
  }

  // Copies final states from the device
  for (i = 0; i < NUM_ITERATORS; i++) {
    // Finds the first iterator with states. 
    // Its pointer will reference the beginning of state memory.
#   if NUM_STATES > 0
    if (0 < props[i].statesize + props[i].algebraic_statesize) {
      cutilSafeCall(cudaMemcpy(props[i].model_states, props[i].gpu.model_states, sizeof(systemstatedata_external), cudaMemcpyDeviceToHost));
      break;
    }
#   endif
  }

}
#endif // #ifdef TARGET_GPU
