// GPU execution kernel that runs each model instance for a number of iterations or until the buffer fills
__GLOBAL__ void exec_kernel_gpu(solver_props *props, int resuming){
  const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;
  
  unsigned int i, num_iterations = 0;
  CDATAFORMAT min_time;
  unsigned int last_iteration[NUM_ITERATORS] = {0};
  unsigned int dirty_states[NUM_ITERATORS] = {0};
  unsigned int ready_outputs[NUM_ITERATORS] = {0};
  int inputs_available = 1;

  if (modelid >= props->num_models) {
    return;
  }

  if (!model_running(props, modelid)) {
    return;
  }

  // Initialize output buffer to store output data
  init_output_buffer(props->ob, modelid);

  // Run up to MAX_ITERATIONS iterations until the output buffer is full or the simulation is complete
  while (1) {
    // Find the nearest next_time and catch up
    min_time = find_min_time(props, modelid);

    // Advance any sampled inputs
    //inputs_available = advance_sampled_inputs(outputs_dirname, min_time, props->modelid_offset, modelid);

    // Buffer any available outputs
    for(i=0;i<NUM_ITERATORS;i++){
      if (ready_outputs[i]) {
#if NUM_OUTPUTS > 0
	buffer_outputs(&props[i], modelid);
#endif
	ready_outputs[i] = 0;
      }
      if (dirty_states[i] && (resuming && props[i].next_time[modelid] == min_time)) {
	solver_writeback(&props[i], modelid);
	dirty_states[i] = 0;
      }
    }

    // Update and postprocess phase: x[t+dt] = f(x[t+dt])
    // Update occurs before the first iteration and after every subsequent iteration.
    for(i=0;i<NUM_ITERATORS;i++){
      if(props[i].running[modelid] && (!resuming || props[i].next_time[modelid] == min_time)){
	dirty_states[i] = 0 == update(&props[i], modelid);
      }	  
      if(props[i].running[modelid] && (resuming && props[i].next_time[modelid] == min_time)){
	dirty_states[i] |= 0 == post_process(&props[i], modelid);
      }
    }

    // Advance the iterator.
    for(i=0;i<NUM_ITERATORS;i++){
      if(props[i].running[modelid] && (resuming && props[i].next_time[modelid] == min_time)){
	// Now time == next_time
	last_iteration[i] = solver_advance(&props[i], modelid);
      }
    }

    for(i=0;i<NUM_ITERATORS;i++){
      if (dirty_states[i] && props[i].next_time[modelid] == min_time) {
	solver_writeback(&props[i], modelid);
	dirty_states[i] = 0;
      }
    }

    // Capture outputs for final iteration
    for(i=0;i<NUM_ITERATORS;i++){
      if (last_iteration[i]) {
	last_iteration[i] = 0;

	pre_process(&props[i], modelid);
	model_flows(props[i].time[modelid], props[i].model_states, props[i].next_states, &props[i], 1, modelid);
	in_process(&props[i], modelid);
	  
	// Updates and postprocess should not write to the output data structure (right?)
	/* update(&props[i], modelid); */
	/* post_process(&props[i], modelid); */

#if NUM_OUTPUTS > 0
	buffer_outputs(&props[i], modelid);
#endif
      }
    }

    // Stop if the maximum number of iterations has been executed
    if (num_iterations++ >= MAX_ITERATIONS) {
      break;
    }

    // Cannot continue if a sampled input with halt condition has no more data
    if(!inputs_available) {
      break;
    }

    // Cannot continue if all the simulation is complete
    if (!model_running(props, modelid)) {
      props->ob->finished[modelid] = 1;
      break;
    }

    // Cannot continue if the output buffer is full
    if (((output_buffer *)(props->ob))->full[modelid]) {
      break;
    }

    resuming = 1;

    // Preprocess phase: x[t] = f(x[t])
    for(i=0;i<NUM_ITERATORS;i++){
      if(props[i].running[modelid] && props[i].time[modelid] == min_time){
	dirty_states[i] = 0 == pre_process(&props[i], modelid);
      }
    }

    for(i=0;i<NUM_ITERATORS;i++){
      if (dirty_states[i] && props[i].time[modelid] == min_time) {
	solver_writeback(&props[i], modelid);
	dirty_states[i] = 0;
      }
    }

    // Main solver evaluation phase, including inprocess.
    // x[t+dt] = f(x[t])
    for(i=0;i<NUM_ITERATORS;i++){
      if(props[i].running[modelid] && props[i].time[modelid] == min_time){
	// TODO check return status
	solver_eval(&props[i], modelid);
	// Now next_time == time + dt
	dirty_states[i] = 1;
	ready_outputs[i] = 1;
	// Run any in-process algebraic evaluations
	in_process(&props[i], modelid);
      }
    }
  }
}
