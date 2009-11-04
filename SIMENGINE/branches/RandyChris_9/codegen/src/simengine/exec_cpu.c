// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, unsigned int modelid){
  Iterator iter;
  Iterator i;

  // Initialize all iterators to running
  for(i=0;i<NUM_ITERATORS;i++){
    props[i].running[modelid] = 1;
  }
  // Run simulation to completion
  while(model_running(props, modelid)){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full
    while(0 == ((output_buffer *)(props->ob))->full[modelid]){
      // Check if simulation is complete
      if(!model_running(props,modelid)){ // WHAT ABOUT IF THERE ARE NO STATES?
	((output_buffer*)(props->ob))->finished[modelid] = 1;
#if NUM_OUTPUTS > 0
	// Log output values for final timestep
	// Run all the model flows to ensure that all intermediates are computed
	for(i=0;i<NUM_ITERATORS;i++){
	  model_flows(props[i].time[modelid], props[i].model_states, props[i].next_states, &props[i], 1, modelid);
	  // Buffer the last values
	  buffer_outputs(&props[i], modelid);
	}
#endif
	break;
      }
		 
      // Find the iterator which is earliest in time
      iter = find_min_time(props, modelid);

      // Write state values back to state storage if they occur before time of iter
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].next_time[modelid] <= props[iter].next_time[modelid] &&
	   props[i].next_time[modelid] > props[i].time[modelid]){
	  int count_current;
	  int count_next;
	  CDATAFORMAT time_current;
	  CDATAFORMAT time_next;

	  // Perform any post process evaluations
	  post_process(&props[i], modelid);
	    // Buffer any outputs
#if NUM_OUTPUTS > 0
	  buffer_outputs(&props[i], modelid);
#endif
	  solver_writeback(&props[i], modelid);
	}
      }

      // Execute solver for one timestep
      solver_eval(&props[iter], modelid);

      // Perform any state updates
      update(&props[iter], modelid);
    }
    // Log outputs from buffer to external api interface
    // All iterators share references to a single output buffer and outputs structure.
    if(0 != log_outputs((output_buffer*)props[0].ob, props[0].outputs, modelid)){
      return ERRMEM;
    }
  }
  
  return SUCCESS;
}
