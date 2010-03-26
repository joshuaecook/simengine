// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, const char *outputs_dirname, double *progress, unsigned int modelid, int resuming){
  unsigned int i;
  CDATAFORMAT min_time;
  unsigned int before_first_iteration = 1;
  unsigned int last_iteration[NUM_ITERATORS] = {0};
  unsigned int dirty_states[NUM_ITERATORS] = {0};
  unsigned int ready_outputs[NUM_ITERATORS] = {0};

  // Initialize all iterators to running
  for(i=0;i<NUM_ITERATORS;i++){
    props[i].running[modelid] = 1;
  }
  if (!resuming) {
    // Call state initialization functions if the initial states
    // have not already been specified.
    init_states(props, modelid);
    for(i=0;i<NUM_ITERATORS;i++){
      solver_writeback(&props[i], modelid);
    }
  }

  // Run simulation to completion
  while(model_running(props, modelid)){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full or the simulation is complete
    while (1) {
      // Find the nearest next_time and catch up
      min_time = find_min_time(props, modelid);

      // Buffer any available outputs
      for(i=0;i<NUM_ITERATORS;i++){
	if (ready_outputs[i]) {
#if NUM_OUTPUTS > 0
	  buffer_outputs(&props[i], modelid);
#endif
	  ready_outputs[i] = 0;
	}
	if (dirty_states[i] && (!before_first_iteration && props[i].next_time[modelid] == min_time)) {
	  solver_writeback(&props[i], modelid);
	  dirty_states[i] = 0;
	}
      }

      // Update and postprocess phase: x[t+dt] = f(x[t+dt])
      // Update occurs before the first iteration and after every subsequent iteration.
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].running[modelid] && (before_first_iteration || props[i].next_time[modelid] == min_time)){
	  dirty_states[i] = 0 == update(&props[i], modelid);
	}	  
	if(props[i].running[modelid] && (!before_first_iteration && props[i].next_time[modelid] == min_time)){
	  dirty_states[i] |= 0 == post_process(&props[i], modelid);
	}
      }

      // Advance the iterator.
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].running[modelid] && (!before_first_iteration && props[i].next_time[modelid] == min_time)){
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

      // Cannot continue if all the simulation is complete
      if (!model_running(props, modelid)) {
	break;
      }

      // Cannot continue if the output buffer is full
      if (((output_buffer *)(props->ob))->full[modelid]) {
	break;
      }

      before_first_iteration = 0;

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
	  if(0 != solver_eval(&props[i], modelid)) {
	    return ERRCOMP;
	  }
	  // Now next_time == time + dt
	  dirty_states[i] = 1;
	  ready_outputs[i] = 1;
	  // Run any in-process algebraic evaluations
	  in_process(&props[i], modelid);
	}
      }
    }

    // Log outputs from buffer to external api interface
    // All iterators share references to a single output buffer and outputs dirname.
    if(0 != log_outputs(props->ob, outputs_dirname, props->modelid_offset, modelid)){
      return ERRMEM;
    }
    progress[modelid] = (props->time[modelid] - props->starttime) / (props->stoptime - props->starttime);
  }
  
  return SUCCESS;
}
