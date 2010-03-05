int exec_parallel_gpu(solver_props *props, double *progress){
  unsigned int i;
  unsigned int modelid;
  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  unsigned int active_models;
  solver_props *device_props;

  num_gpu_threads = GPU_BLOCK_SIZE < props->num_models ? GPU_BLOCK_SIZE : props->num_models;
  num_gpu_blocks = (props->num_models + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;

  // Initialize all iterators to running
  active_models = 1;
  for(modelid = 0; modelid < props->num_models; modelid++){
    for(i=0;i<NUM_ITERATORS;i++){
      props[i].running[modelid] = 1;
    }
  }
  
  // Initialize GPU device memory for all solvers (returns pointer to device memory)
  device_props = gpu_init_props(props);

  while(active_models){
    // Execute models on the GPU
    exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(device_props);
    // Copy data back to the host
    cutilSafeCall(cudaMemcpy(props->ob, props->gpu.ob, props->ob_size, cudaMemcpyDeviceToHost));
    cutilSafeCall(cudaMemcpy(props->time, props->gpu.time, props->num_models * sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));

    active_models = 0;
    // Copy data to external api interface
    for(modelid = 0; modelid < props->num_models; modelid++){
      if(0 != log_outputs(props->ob, props->outputs_dirname, props->modelid_offset, modelid)) return ERRMEM;
      progress[modelid] = (props->time[modelid] - props->starttime) / (props->stoptime - props->starttime);
      active_models |= !props->ob->finished[modelid];
    }
  }

  // Copy any remaining data back from GPU
  gpu_finalize_props(props);

  return SUCCESS;
}
