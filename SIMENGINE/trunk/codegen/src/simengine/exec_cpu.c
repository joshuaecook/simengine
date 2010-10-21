// Run a single model to completion on a single processor core
void exec_writeback (CDATAFORMAT min_time, solver_props *props, unsigned int *dirty_states, unsigned int modelid) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if (dirty_states[i] && props[i].time[modelid] == min_time) {
      solver_writeback(&props[i], modelid);
      dirty_states[i] = 0;
    }
  }
}

void exec_buffer_outputs (CDATAFORMAT min_time, solver_props *props, unsigned int *ready_outputs, unsigned int *dirty_states, unsigned int modelid, int resuming) {
  int i;
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
}

void exec_update_and_postprocess (CDATAFORMAT min_time, solver_props *props, unsigned int *dirty_states, unsigned int modelid, int resuming) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if(props[i].running[modelid] && (!resuming || props[i].next_time[modelid] == min_time)){
      dirty_states[i] = 0 == update(&props[i], modelid);
    }	  
    if(props[i].running[modelid] && (resuming && props[i].next_time[modelid] == min_time)){
      dirty_states[i] |= 0 == post_process(&props[i], modelid);
    }
  }
}

void exec_preprocess (CDATAFORMAT min_time, solver_props *props, unsigned int *dirty_states, unsigned int modelid) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if(props[i].running[modelid] && props[i].time[modelid] == min_time){
      dirty_states[i] = 0 == pre_process(&props[i], modelid);
    }
  }
}

int exec_solver_and_inprocess (CDATAFORMAT min_time, solver_props *props, unsigned int *ready_outputs, unsigned int *dirty_states, unsigned int modelid) {
  int i;
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
  return SUCCESS;
}

void exec_final_outputs (solver_props *props, unsigned int modelid) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if (props[i].last_iteration[modelid]) {
      props[i].last_iteration[modelid] = 0;

      pre_process(&props[i], modelid);
#if defined X_IR_SPIL
      model_flows(props[i].time[modelid], props[i].model_states, props[i].next_states, props[i].model_inputs, props[i].model_outputs, &props[i], 1, modelid);
#else
      model_flows(props[i].time[modelid], props[i].model_states, props[i].next_states, &props[i], 1, modelid);
#endif
      in_process(&props[i], modelid);
	  
      // Updates and postprocess should not write to the output data structure
      // the output data structure holds outputs from the previous iteration and updates/postprocess set values
      // that will be written to output data by the solver flow of the next iteration
      /* update(&props[i], modelid); */
      /* post_process(&props[i], modelid); */

#if NUM_OUTPUTS > 0
      buffer_outputs(&props[i], modelid);
#endif
    }
  }
}

int exec_cpu(solver_props *props, const char *outputs_dirname, double *progress, unsigned int modelid, int resuming){
  unsigned int i;
  CDATAFORMAT min_time;
  unsigned int dirty_states[NUM_ITERATORS] = {0};
  unsigned int ready_outputs[NUM_ITERATORS] = {0};
  int inputs_available = 1;
  unsigned int iterid = NUM_ITERATORS - 1;

  // Initialize all iterators to running
  for(i=0;i<NUM_ITERATORS;i++){
    props[i].running[modelid] = 1;
  }

  // Initialize a temporary output buffer
  init_output_buffer(&global_ob[global_ob_idx[modelid]], modelid);

  // Run simulation to completion
  while(model_running(props, modelid) && inputs_available){
    // Update the progress file with % completion for this model
    progress[modelid] = (props[iterid].time[modelid] - props[iterid].starttime) / (props[iterid].stoptime - props[iterid].starttime);

    // Find the nearest next_time and catch up
    min_time = find_min_time(props, modelid);

    // Advance any sampled inputs
    inputs_available = 1;
#if NUM_SAMPLED_INPUTS > 0
    for (i=NUM_CONSTANT_INPUTS; i<NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS; i++) {
      sampled_input_t *input = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(i)];
      if (!advance_sampled_input(input, min_time, props->modelid_offset, modelid)) {
	// If unable to advance, attempt to buffer more input data.
	inputs_available &= read_sampled_input(input, min_time, outputs_dirname, i, props->num_models, props->modelid_offset, modelid);
      }
    }
#endif

    // Outputs: out[t] = f(x[t])
    // Inputs: in[t] = f(x[t],out[t])

    // Buffer any available outputs
    exec_buffer_outputs(min_time,props,ready_outputs,dirty_states,modelid,resuming);

#if NUM_OUTPUTS > 0
    // Log outputs if the buffer is full
    if (global_ob[global_ob_idx[modelid]].full[modelid]) {
      // Log outputs from buffer to external api interface
      // All iterators share references to a single output buffer and outputs dirname.
      if(0 != log_outputs(outputs_dirname, props->modelid_offset, modelid)){
	return ERRMEM;
      }
      // Reinitialize a temporary output buffer
      init_output_buffer(&global_ob[global_ob_idx[modelid]], modelid);
    }
#endif


    // Update and postprocess phase: x[t+dt] = f(x[t+dt])
    // Update occurs before the first iteration and after every subsequent iteration.
    exec_update_and_postprocess(min_time,props,dirty_states,modelid,resuming);

    // Advance the iterator.
    solver_advance(min_time,props,modelid,resuming);

    exec_writeback(min_time,props,dirty_states,modelid);

    // Capture outputs for final iteration
    exec_final_outputs(props,modelid);
     
    // Cannot continue if a sampled input with halt condition has no more data
    if(!inputs_available) { break;}

    // Cannot continue if all the simulation is complete
    if (!model_running(props, modelid)) {break;}

    resuming = 1;

    // Preprocess phase: x[t] = f(x[t])
    exec_preprocess(min_time,props,dirty_states,modelid);

    exec_writeback(min_time,props,dirty_states,modelid);

    // Main solver evaluation phase, including inprocess.
    // x[t+dt] = f(x[t])
    if (SUCCESS != exec_solver_and_inprocess(min_time, props, ready_outputs, dirty_states, modelid)) {
      return ERRCOMP;
    }
  }
  
  // Log any remaining outputs
  // Log outputs from buffer to external api interface
  // All iterators share references to a single output buffer and outputs dirname.
  if(0 != log_outputs(outputs_dirname, props->modelid_offset, modelid)){
    return ERRMEM;
  }

  // Update the progress file with % completion for this model
  progress[modelid] = (props[iterid].time[modelid] - props[iterid].starttime) / (props[iterid].stoptime - props[iterid].starttime);

  return SUCCESS;
}
