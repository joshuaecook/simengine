// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, unsigned int modelid){
  unsigned int i;
  Iterator iter;
  CDATAFORMAT min_time;

  // Initialize all iterators to running
  for(i=0;i<NUM_ITERATORS;i++){
    iter = ITERATORS[i];
    props[iter].running[modelid] = 1;
  }
  // Run simulation to completion
  while(model_running(props, modelid)){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full
    while(0 == ((output_buffer *)(props->ob))->full[modelid]){
      // Run solvers for all iterators that need to advance
      for(i=0;i<NUM_ITERATORS;i++){
	iter = ITERATORS[i];
	if(props[iter].next_time[modelid] == props[iter].time[modelid]){
	  solver_eval(&props[iter], modelid);
	  if(!props[iter].running[modelid]){
	    model_flows(props[iter].time[modelid], props[iter].model_states, props[iter].next_states, &props[i], 1, modelid);
	  }
	  update(&props[iter], modelid);
	}
      }

      // Perform any post process evaluations (BEFORE OR AFTER BUFFER OUTPUTS?)
      if(model_running(props, modelid)){
	min_time = find_min_time(props, modelid);
	for(i=0;i<NUM_ITERATORS;i++){
	  iter = ITERATORS[i];
	  if(props[iter].next_time[modelid] == min_time &&
	     props[iter].next_time[modelid] > props[iter].time[modelid]){
	    post_process(&props[iter], modelid);
	  }
	}
      }

#if NUM_OUTPUTS > 0
      // Buffer outputs for all iterators that have values to output
      // This isn't conditional on running to make sure it outputs a value for the last time
      for(i=0;i<NUM_ITERATORS;i++){
	int buffer_ready = 1;
	for(unsigned int j=0;j<NUM_ITERATORS;j++){
	  if(props[ITERATORS[j]].time[modelid] > props[ITERATORS[i]].time[modelid]){
	    // Some iterator has run out ahead, don't buffer again
	    buffer_ready = 0;
	    break;
	  }
	}
	if(buffer_ready){
	  buffer_outputs(&props[ITERATORS[i]], modelid);
	}
      }
#endif
	
      // Write state values back to state storage
      if(model_running(props, modelid)){
	for(i=0;i<NUM_ITERATORS;i++){
	  iter = ITERATORS[i];
	  if(props[iter].next_time[modelid] == min_time &&
	     props[iter].next_time[modelid] > props[iter].time[modelid]){
	    solver_writeback(&props[iter], modelid);
	  }
	}
      }
      else{
	// Model is complete, make sure to break out of the loop
	break;
      }
    }
    // Log outputs from buffer to external api interface
    // All iterators share references to a single output buffer and outputs structure.
    if(0 != log_outputs((output_buffer*)props[0].ob, props[0].outputs, modelid)){
      return ERRMEM;
    }
  }
  
  return SUCCESS;
}
