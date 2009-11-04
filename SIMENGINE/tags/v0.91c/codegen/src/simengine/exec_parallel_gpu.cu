#if defined(TARGET_GPU)
EXTERN_C unsigned long long inline getnanos(void);


int exec_parallel_gpu(INTEGRATION_MEM *mem, solver_props *props, simengine_output *outputs){
  int ret = SUCCESS;

  unsigned int active_models = NUM_MODELS;

  output_buffer *device_ob = (output_buffer *)props->gpu.ob;
  output_buffer *pinned_ob;
  cutilSafeCall( cudaHostAlloc((void **)&pinned_ob, sizeof(output_buffer), cudaHostAllocPortable) );

  unsigned int kid = 0; // Kernel id
  unsigned int aflight = NO;

  cudaStream_t stream[2];
  cutilSafeCall( cudaStreamCreate(&stream[0]) );
  cutilSafeCall( cudaStreamCreate(&stream[1]) );

  cudaEvent_t kernel_start, kernel_stop, memcpy_start, memcpy_stop;
  unsigned long long logging_start, logging_elapsed;
  float kernel_elapsed, memcpy_elapsed;
  cutilSafeCall( cudaEventCreate(&kernel_start) );
  cutilSafeCall( cudaEventCreate(&kernel_stop) );
  cutilSafeCall( cudaEventCreate(&memcpy_start) );
  cutilSafeCall( cudaEventCreate(&memcpy_stop) );

  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  num_gpu_threads = GPU_BLOCK_SIZE < NUM_MODELS ? GPU_BLOCK_SIZE : NUM_MODELS;
  num_gpu_blocks = (NUM_MODELS + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;
	     
  dim3 block(num_gpu_threads, 1, 1);
  dim3 grid(num_gpu_blocks, 1, 1);

    PRINTF("kernel geometry <<<(%dx%dx%d),(%dx%dx%d),%zd>>>\n",
	   grid.x, grid.y, grid.z,
	   block.x, block.y, block.z);



{
    unsigned int modelid;
    unsigned int thread_num = omp_get_thread_num();
    unsigned int num_threads = omp_get_num_threads();
    
    unsigned int models_per_thread = NUM_MODELS/num_threads;
    unsigned int extra_models = NUM_MODELS%num_threads;



    while(SUCCESS == ret && active_models)
	{
	if (aflight) 
	    { 
            // A kernel is executing; wait for it to complete.
		{
		cutilSafeCall( cudaStreamSynchronize(stream[kid]) );
		aflight = NO;

		cutilSafeCall( cudaEventElapsedTime(&kernel_elapsed, kernel_start, kernel_stop) );

#               if defined _DEBUG
		PRINTF("kernel %d elapsed %.4fs\n", kid, kernel_elapsed / 1.0E3);
#               endif		

		// Asynchronously copies the filled output buffers back to the host.
		cutilSafeCall( cudaEventRecord(memcpy_start, stream[kid]) );
		cutilSafeCall( cudaMemcpyAsync(pinned_ob, device_ob + kid, sizeof(output_buffer), cudaMemcpyDeviceToHost, stream[kid]) );
		cutilSafeCall( cudaEventRecord(memcpy_stop, stream[kid]) );

		// Synchronously copies the `finished' flags between device buffers.
		cutilSafeCall( cudaMemcpy(device_ob + (1^kid), device_ob + kid, props->num_models * sizeof(unsigned int), cudaMemcpyDeviceToDevice) );
		cutilSafeCall( cudaStreamSynchronize(0) );

		// Launches another kernel.
		// At this point, all models may be finished, so the last kernel launch is wasted.
		kid ^= 1;
		/* PRINTF("launching next kernel<<<(%dx%dx%d),(%dx%dx%d),%zd>>> with %d active_models (id %d)\n", */
		/*     grid.x, grid.y, grid.z, */
		/*     block.x, block.y, block.z, */
		/*     props->gpu.shmem_per_block, */
		/*     active_models, kid); */
		cutilSafeCall( cudaEventRecord(kernel_start, stream[kid]) );
		exec_kernel_gpu<<< grid, block, 0, stream[kid] >>>(mem, kid);
		cutilSafeCall( cudaEventRecord(kernel_stop, stream[kid]) );
		aflight = YES;

		// Waits for the asyc copy to complete.
		cutilSafeCall( cudaStreamSynchronize(stream[1^kid]) );
		active_models = NUM_MODELS;

		cutilSafeCall( cudaEventElapsedTime(&memcpy_elapsed, memcpy_start, memcpy_stop) );
#               if defined _DEBUG
		PRINTF("memcpy %d elapsed %.4f s; %.4f MiB; %.4f GiBps\n", (1^kid), memcpy_elapsed / 1.0E3, sizeof(output_buffer) / (1.0*(1<<20)), sizeof(output_buffer) / (1.0*(1<<30)) / (memcpy_elapsed / 1.0E3));
#               endif

		logging_start = getnanos();
		}

	    /* PRINTF("logging for thread %d kernel %d\n", thread_num, (1^kid)); */
	    // Copies output data to external API data structures.
	    for (modelid = thread_num * models_per_thread; modelid < (1+thread_num) * models_per_thread; modelid++)
		{
		if (pinned_ob->finished[modelid]) 
		    {
		    active_models--;
		    }
		if (SUCCESS != log_outputs(pinned_ob, outputs, modelid)) 
		    {
		    ret = ERRMEM;
		    break;
		    }
		}
	    // If the number of models is not an even multiple of the number of cores
	    // there will be one additional batch of models, with fewer threads than
	    // the number of cores
	    if (thread_num < extra_models) 
		{
		modelid = NUM_MODELS - extra_models + thread_num;
		if (pinned_ob->finished[modelid]) 
		    {
		    active_models--;
		    }
		if (SUCCESS != log_outputs(pinned_ob, outputs, modelid)) 
		    {
		    ret = ERRMEM;
		    }
		}

#           if defined _DEBUG
		{
		float total_consumption = 0.0f;
		logging_elapsed = getnanos() - logging_start;
		for (modelid=0; modelid<NUM_MODELS; modelid++)
		  { total_consumption += output_buffer_consumption[modelid]; }
		PRINTF("logging %d elapsed %.4fs\n", (1^kid), logging_elapsed / 1.0E9);
		PRINTF("%.f%% average buffer consumption\n", 1.0e2f * total_consumption / NUM_MODELS);
		}
#           endif
	    }
	else if (active_models)
	    {
		{
		// No active kernels; launch one.
		/* PRINTF("launching first kernel<<<(%dx%dx%d),(%dx%dx%d),%zd>>> with %d active_models (id %d)\n", */
		/*     grid.x, grid.y, grid.z, */
		/*     block.x, block.y, block.z, */
		/*     props->gpu.shmem_per_block, */
		/*     active_models, kid); */
		cutilSafeCall( cudaEventRecord(kernel_start, stream[kid]) );
		exec_kernel_gpu<<< grid, block, 0, stream[kid] >>>(mem, kid);
		cutilSafeCall( cudaEventRecord(kernel_stop, stream[kid]) );
		aflight = YES;
		}
	    }
	}

    if (aflight)
	{ 
        // Awaits the ultimate kernel completion.
	    {
	    cutilSafeCall( cudaStreamSynchronize(stream[kid]) );
	    aflight = NO;
	    cutilSafeCall( cudaMemcpy(pinned_ob, device_ob + kid, sizeof(output_buffer), cudaMemcpyDeviceToHost) );
	    }

	/* PRINTF("logging for thread %d kernel %d\n", thread_num, kid); */
	// Copies output data to external API data structures.
	for (modelid = thread_num * models_per_thread; modelid < (1+thread_num) * models_per_thread; modelid++)
	    {
	    if (pinned_ob->finished[modelid]) 
		{
		active_models--;
		}
	    if (SUCCESS != log_outputs(pinned_ob, outputs, modelid)) 
		{
		ret = ERRMEM;
		break;
		}
	    }
	// If the number of models is not an even multiple of the number of cores
	// there will be one additional batch of models, with fewer threads than
	// the number of cores
	if (thread_num < extra_models) 
	    {
	    modelid = NUM_MODELS - extra_models + thread_num;
	    if (pinned_ob->finished[modelid]) 
		{
		active_models--;
		}
	    if (SUCCESS != log_outputs(pinned_ob, outputs, modelid)) 
		{
		ret = ERRMEM;
		}
	    }
	}
    // Host threads join at this point.
}


  cutilSafeCall( cudaStreamDestroy(stream[0]) );
  cutilSafeCall( cudaStreamDestroy(stream[1]) );

  cutilSafeCall( cudaEventDestroy(kernel_start) );
  cutilSafeCall( cudaEventDestroy(kernel_stop) );
  cutilSafeCall( cudaEventDestroy(memcpy_start) );
  cutilSafeCall( cudaEventDestroy(memcpy_stop) );

  // Copy final times and states from GPU
  cutilSafeCall(cudaMemcpy(props->time, props->gpu.time, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
  cutilSafeCall(cudaMemcpy(props->model_states, props->gpu.model_states, props->statesize*props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));

  cutilSafeCall( cudaFreeHost(pinned_ob) );

  return ret;
}

EXTERN_C
unsigned long long inline getnanos(void){
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return (unsigned long long) ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

#endif // defined(TARGET_GPU)
