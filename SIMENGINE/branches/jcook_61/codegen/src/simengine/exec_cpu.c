// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, const char *outputs_dirname, double *progress, unsigned int modelid){
  unsigned int i;
  CDATAFORMAT min_time;
  unsigned int before_first_iteration = 1;
  unsigned int last_iteration[NUM_ITERATORS] = {0};
  unsigned int dirty[NUM_ITERATORS] = {0};

  // Initialize all iterators to running
  for(i=0;i<NUM_ITERATORS;i++){
    props[i].running[modelid] = 1;
  }
  // TODO Initialize non-constant state initial values

  // Run simulation to completion
  while(model_running(props, modelid)){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full or the simulation is complete
    while (1) {
      // Find the nearest next_time and catch up
      min_time = find_min_time(props, modelid);

      // Update and postprocess phase: x[t+dt] = f(x[t+dt])
      // Always occurs before the first iteration and after every subsequent iteration.
      for(i=0;i<NUM_ITERATORS;i++){
	dirty[i] = 0;
	if(props[i].running[modelid] && (before_first_iteration || props[i].time[modelid] < min_time)){
	  update(&props[i], modelid);
	  dirty[i] = 1;
	}	  
	if(props[i].running[modelid] && props[i].time[modelid] < min_time){
	  post_process(&props[i], modelid);
	  dirty[i] = 1;
	}
      }
      for(i=0;i<NUM_ITERATORS;i++){
	if (dirty[i]) {
	  solver_writeback(&props[i], modelid);
	}
      }
      before_first_iteration = 0;

      // Write any available output data to the buffer
      // and advance the iterator.
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].running[modelid] && props[i].time[modelid] < min_time){
#if NUM_OUTPUTS > 0
	  buffer_outputs(&props[i], modelid);
#endif
	  // Now time == next_time
	  iterator_advance(&props[i], modelid);
	}
      }

      // Capture outputs for final iteration
      for(i=0;i<NUM_ITERATORS;i++){
	if (last_iteration[i]) {
	  model_flows(props[i].time[modelid], props[i].model_states, props[i].next_states, &props[i], 1, modelid);
#if NUM_OUTPUTS > 0
	  buffer_outputs(&props[i], modelid);
#endif
	  last_iteration[i] = 0;
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

      // Preprocess phase: x[t] = f(x[t])
      for(i=0;i<NUM_ITERATORS;i++){
	dirty[i] = 0;
	if(props[i].running[modelid] && props[i].time[modelid] == min_time){
	  pre_process(&props[i], modelid);
	  dirty[i] = 1;
	}
      }
      for(i=0;i<NUM_ITERATORS;i++){
	if (dirty[i]) {
	  solver_writeback(&props[i], modelid);
	}
      }

      // Main solver evaluation phase, including inprocess.
      // x[t+dt] = f(x[t])
      for(i=0;i<NUM_ITERATORS;i++){
	dirty[i] = 0;
	if(props[i].running[modelid] && props[i].time[modelid] == min_time){
	  if(0 != solver_eval(&props[i], modelid)) {
	    return ERRCOMP;
	  }
	  // Now next_time == time + dt
	  dirty[i] = 1;
	  if(props[i].next_time[modelid] >= props->stoptime){
	    last_iteration[i] = 1;
	  }
	  // Run any in-process algebraic evaluations
	  in_process(&props[i], modelid);
	}
      }

      // Write state values back to state storage
      for(i=0;i<NUM_ITERATORS;i++){
	if (dirty[i]) {
	  solver_writeback(&props[i], modelid);
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
