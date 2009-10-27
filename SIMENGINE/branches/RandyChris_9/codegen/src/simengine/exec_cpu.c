#ifndef TARGET_GPU
// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, simengine_output *outputs, unsigned int modelid){
  Iterator iter;
  Iterator i;

  for (i = 0; i < NUM_ITERATORS; i++)
    { props[i].running[modelid] = 1; }

  // Run simulation to completion
  while(model_running(props, modelid)){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full
    while(0 == ((output_buffer *)(props->ob))->full[modelid]){
      // Check if simulation is complete (or produce only a single output if there are no states)
      if(!props->running[modelid] || props->statesize == 0){
	props->running[modelid] = 0;
	((output_buffer*)(props->ob))->finished[modelid] = 1;
#if NUM_OUTPUTS > 0
	// Log output values for final timestep
	// Run the model flows to ensure that all intermediates are computed
	model_flows(props->time[modelid], props->model_states, props->next_states, props, 1, modelid);
	// Buffer the last values
	buffer_outputs(props, modelid);
#endif
	break;
      }
		 
      // Find the iterator which is earliest in time
      iter = find_min_t(props, modelid);
      CDATAFORMAT prev_time = props[iter].time[modelid];

      // Write state values back to state storage if they occur before time of iter
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].time <= props[iter].time){
	  solver_writeback(&props[i], modelid);
	  // Perform any post process evaluations
	  post_process(&props[iter], modelid);
	}
      }

      // Execute solver for one timestep
      solver_eval(&props[iter], modelid);

      // Perform any state updates
      update(&props[iter], modelid);

#if NUM_OUTPUTS > 0
      // Store a set of outputs only if the sovler made a step
      if (props[iter].time[modelid] > prev_time) {
	buffer_outputs(&props[iter], modelid);
      }
#endif
    }
    // Log outputs from buffer to external api interface
    if(0 != log_outputs((output_buffer*)props->ob, outputs, modelid)){
      return ERRMEM;
    }
  }
  
  return SUCCESS;
}
#endif
