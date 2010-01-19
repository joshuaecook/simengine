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

      // Perform any pre-process evaluations
      if(model_running(props, modelid)){
	min_time = find_min_time(props, modelid);
	for(i=0;i<NUM_ITERATORS;i++){
	  if(props[i].next_time[modelid] == min_time &&
	     props[i].next_time[modelid] == props[i].time[modelid]){
	    pre_process(&props[i], modelid);
	  }
	}
      }

      // Run solvers for all iterators that need to advance
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].next_time[modelid] == props[i].time[modelid]){
	  solver_eval(&props[i], modelid);
	  if(!props[i].running[modelid]){
	    model_flows(props[i].time[modelid], props[i].model_states, props[i].next_states, &props[i], 1, modelid);
	  }
	  // Run any in-process algebraic evaluations
	  in_process(&props[i], modelid);
	  // Run updates
	  update(&props[i], modelid);
	}
      }

      // Perform any post process evaluations (BEFORE OR AFTER BUFFER OUTPUTS?)
      if(model_running(props, modelid)){
	min_time = find_min_time(props, modelid);
	for(i=0;i<NUM_ITERATORS;i++){
	  if(props[i].next_time[modelid] == min_time &&
	     props[i].next_time[modelid] > props[i].time[modelid]){
	    post_process(&props[i], modelid);
	  }
	}
      }

#if NUM_OUTPUTS > 0
      // Buffer outputs for all iterators that have values to output
      // This isn't conditional on running to make sure it outputs a value for the last time
      for(i=0;i<NUM_ITERATORS;i++){
	int buffer_ready = 1;
	for(unsigned int j=0;j<NUM_ITERATORS;j++){
	  if(props[j].time[modelid] > props[i].time[modelid]){
	    // Some iterator has run out ahead, don't buffer again
	    buffer_ready = 0;
	    break;
	  }
	}
	if(buffer_ready){
	  buffer_outputs(&props[i], modelid);
	}
      }
#endif
	
      // Write state values back to state storage
      if(model_running(props, modelid)){
	for(i=0;i<NUM_ITERATORS;i++){
	  if(props[i].next_time[modelid] == min_time &&
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
