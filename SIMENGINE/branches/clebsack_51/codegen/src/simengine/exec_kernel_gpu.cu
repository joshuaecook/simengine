// exec_kernel_gpu.cu 
/**********************************************************************/
#if defined(TARGET_GPU)
// GPU execution kernel that runs each model instance for a number of iterations or until the buffer fills
__GLOBAL__ void exec_kernel_gpu(INTEGRATION_MEM *mem){
  const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;
  
  unsigned int num_iterations;
	     
  if (modelid < NUM_MODELS) {

    // Initialize output buffer to store output data
    init_output_buffer((output_buffer*)(mem->props->ob), modelid);
    
    // Run up to MAX_ITERATIONS for each model
    for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){
      // Check if simulation finished previously
      if(((output_buffer*)(mem->props->ob))->finished[modelid]){
	// (threads are launched in batches on the GPU and not all will complete at the
	// same time with variable timestep solvers)
	break;
      }
      // Check if the simulation just finished (or if there are no states)
      if(!mem->props->running[modelid] || mem->props->statesize == 0){
	mem->props->running[modelid] = 0;
	((output_buffer*)(mem->props->ob))->finished[modelid] = 1;
	atomicDec(&((output_buffer*)(mem->props->ob))->active_models, NUM_MODELS);
#if NUM_OUTPUTS > 0
	// Log output values for final timestep
	// Run the model flows to ensure that all intermediates are computed, mem->k1 is borrowed from the solver as scratch for ignored dydt values
	model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);
	// Buffer the last values
	buffer_outputs(mem->props->time[modelid],((output_data*)mem->props->outputs), ((output_buffer*)mem->props->ob), modelid);
#endif
	break;
      }
      
      CDATAFORMAT prev_time = mem->props->time[modelid];

      // Execute solver for one timestep
      SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);

#if NUM_OUTPUTS > 0
      // Store a set of outputs only if the sovler made a step
      if (mem->props->time[modelid] > prev_time) {
	buffer_outputs(prev_time, (output_data*)mem->props->outputs, (output_buffer*)mem->props->ob, modelid);
      }
#endif
      
      // Stop if the output buffer is full
      if(((output_buffer*)(mem->props->ob))->full[modelid]) { 
	break; 
      }
    }
  }
}
#endif
/**********************************************************************/
