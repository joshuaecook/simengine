// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, const char *outputs_dirname, double *progress, unsigned int modelid){
  unsigned int i;
  CDATAFORMAT min_time;

  // Initialize all iterators to running
  for(i=0;i<NUM_ITERATORS;i++){
    props[i].running[modelid] = 1;
  }
  // Run simulation to completion
  while(model_running(props, modelid)){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full
    while(model_running(props, modelid) && 0 == ((output_buffer *)(props->ob))->full[modelid]){

      // Run any pre-process algebraic evaluations
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].next_time[modelid] == props[i].time[modelid]){
	  pre_process(&props[i], modelid);
	}
      }

      // Run solvers for all iterators that need to advance
      for(i=0;i<NUM_ITERATORS;i++){
	if(props[i].running[modelid] && props[i].next_time[modelid] == props[i].time[modelid]){
	  if(0 != solver_eval(&props[i], modelid)) {
	    return ERRCOMP;
	  }
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
      if(model_running(props,modelid)){
	min_time = find_min_time(props, modelid);
	for(i=0;i<NUM_ITERATORS;i++){
	  if(props[i].running[modelid] && 
	     props[i].next_time[modelid] == min_time &&
	     props[i].next_time[modelid] != props[i].time[modelid]){
	    solver_writeback(&props[i], modelid);
	  }
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
