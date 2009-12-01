int exec_parallel_gpu(solver_props *props){
  unsigned int i;
  unsigned int modelid;
  Iterator iter;
  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  solver_props *device_props;
  num_gpu_threads = GPU_BLOCK_SIZE < NUM_MODELS ? GPU_BLOCK_SIZE : NUM_MODELS;
  num_gpu_blocks = (NUM_MODELS + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;

  // Initialize all iterators to running
  for(modelid = 0; modelid < NUM_MODELS; modelid++){
    for(i=0;i<NUM_ITERATORS;i++){
      iter = ITERATORS[i];
      props[iter].running[modelid] = 1;
    }
  }

  // Initialize GPU device memory for all solvers (returns pointer to device memory)
  device_props = gpu_init_props(props);

  while(((output_buffer*)props->ob)->active_models){
    // Execute models on the GPU
    exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(device_props);
    // Copy data back to the host
    cutilSafeCall(cudaMemcpy(props->ob, props->gpu.ob, props->ob_size, cudaMemcpyDeviceToHost));

    // Copy data in parallel to external api interface
    for(modelid = 0; modelid < props->num_models; modelid++){
      if(0 != log_outputs((output_buffer*)props->ob, outputs, modelid))
	return ERRMEM;
    }
  }

  // Copy any remaining data back from GPU
  gpu_finalize_props(props);

  return SUCCESS;
}
