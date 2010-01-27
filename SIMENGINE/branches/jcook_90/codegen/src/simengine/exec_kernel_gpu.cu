// GPU execution kernel that runs each model instance for a number of iterations or until the buffer fills
__GLOBAL__ void exec_kernel_gpu(solver_props *props){
  const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;
  
  unsigned int num_iterations, i;
  Iterator iter;
  CDATAFORMAT min_time;
	     
  if (modelid < NUM_MODELS) {

    // Initialize output buffer to store output data
    init_output_buffer(props->ob, modelid);
    
    // Run up to MAX_ITERATIONS for each model
    for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){
      // Check if simulation finished previously
      if(props->ob->finished[modelid]){
	// (threads are launched in batches on the GPU and not all will complete at the
	// same time with variable timestep solvers)
	break;
      }
      else if (!model_running(props, modelid)) {
	props->ob->finished[modelid] = 1;
	break;
      }

      // Run any pre-process evaluations
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].next_time[modelid] == props[i].time[modelid]){
	  pre_process(&props[i], modelid);
	}
      }

      // Run solvers for all iterators that need to advance
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[iter].running[modelid] && props[i].next_time[modelid] == props[i].time[modelid]){
	  solver_eval(&props[i], modelid);
	  if(!props[i].running[modelid]){
	    model_flows(props[i].time[modelid], props[i].model_states, props[i].next_states, &props[i], 1, modelid);
	  }
	  // Run any in-process algebraic evaluations
	  in_process(&props[i], modelid);
	  // Run updates
	  update(&props[i], modelid);
	  // Run any post-process algebraic evaluations
	  post_process(&props[i], modelid);
#if NUM_OUTPUTS > 0
	  // Write outputs to buffer
	  buffer_outputs(&props[i], modelid);
#endif
	}
      }

      // Write state values back to state storage
      if(model_running(props, modelid)){
	for(i=0;i<NUM_ITERATORS;i++){
	  if(props[iter].running[modelid] &&
	     props[i].next_time[modelid] == min_time &&
	     props[i].next_time[modelid] > props[i].time[modelid]){
	    solver_writeback(&props[i], modelid);
	  }
	}
      }
      else{
	// Model is complete, make sure to break out of the loop
	break;
      }

      // Stop if the output buffer is full
      if(props->ob->full[modelid]) { 
	break; 
      }
    }
  }
}
