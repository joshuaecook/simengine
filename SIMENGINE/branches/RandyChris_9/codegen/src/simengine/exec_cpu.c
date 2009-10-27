// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, simengine_output *outputs, unsigned int modelid){
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
	if(props[i].time <= props[iter].time){
	  solver_writeback(&props[i], modelid);
	  // Perform any post process evaluations
	  post_process(&props[i], modelid);
#if NUM_OUTPUTS > 0
	  // Buffer any outputs
	  buffer_outputs(&props[i], modelid);
#endif
	}
      }

      // Execute solver for one timestep
      solver_eval(&props[iter], modelid);

      // Perform any state updates
      update(&props[iter], modelid);
    }
    // Log outputs from buffer to external api interface
    if(0 != log_outputs((output_buffer*)props->ob, outputs, modelid)){
      return ERRMEM;
    }
  }
  
  return SUCCESS;
}
