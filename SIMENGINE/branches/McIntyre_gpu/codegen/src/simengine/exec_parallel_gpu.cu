#if defined(TARGET_GPU)
int exec_parallel_gpu(INTEGRATION_MEM *mem, solver_props *props, simengine_output *outputs){
  int ret = SUCCESS;
  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  num_gpu_threads = GPU_BLOCK_SIZE < NUM_MODELS ? GPU_BLOCK_SIZE : NUM_MODELS;
  num_gpu_blocks = (NUM_MODELS + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;
	     
  // Initialize omp with one thread per processor core
  omp_set_num_threads(omp_get_num_procs());

  // Log outputs using parallel host threads
#pragma omp parallel
  {
    int status;
    unsigned int modelid;
    unsigned int thread_num = omp_get_thread_num();
    unsigned int num_threads = omp_get_num_threads();
    
    unsigned int models_per_thread = NUM_MODELS/num_threads;
    unsigned int extra_models = NUM_MODELS%num_threads;
		 
    while(SUCCESS == ret && ((output_buffer*)props->ob)->active_models){
      // Only Host thread 0 can interact with the GPU
      if(0 == thread_num){
	// Execute models on the GPU
	exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(mem);
	// Copy data back to the host
	cutilSafeCall(cudaMemcpy(props->ob, props->gpu.ob, props->ob_size, cudaMemcpyDeviceToHost));
      }

      // Make sure all threads wait for GPU to produce data
#pragma omp barrier

      // Copy data in parallel to external api interface
      for(modelid = thread_num*models_per_thread; modelid < (thread_num+1)*models_per_thread; modelid++){
	status = log_outputs((output_buffer*)props->ob, outputs, modelid);
	if(SUCCESS != status){
	  ret = ERRMEM;
ret = 9;
	  break;
	}
      }
      // If the number of models is not an even multiple of the number of cores
      // there will be one additional batch of models, with fewer threads than
      // the number of cores
      if(thread_num < extra_models){   
	for (modelid = 0; modelid < semeta.num_models; ++modelid) {
	  status = log_outputs((output_buffer*)props->ob, outputs, modelid);
	  if (SUCCESS != status){
	    ret = ERRMEM;
ret = 20;
	    break;
	  }
	}
      }
    } // Host threads implicitly join here
  }
  // Copy final times ans states from GPU
  cutilSafeCall(cudaMemcpy(props->time, props->gpu.time, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
  cutilSafeCall(cudaMemcpy(props->model_states, props->gpu.model_states, props->statesize*props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));

  return ret;
}
#endif // defined(TARGET_GPU)
