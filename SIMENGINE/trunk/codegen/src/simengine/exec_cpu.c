void exec_buffer_outputs (CDATAFORMAT min_time, solver_props *props, unsigned int modelid, int resuming);
void exec_writeback (CDATAFORMAT min_time, solver_props *props, unsigned int modelid);
void exec_update_and_postprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid, int resuming);
void exec_preprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid);
int exec_solver_and_inprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid);
void exec_final_outputs (solver_props *props, unsigned int modelid);

// Run a single model to completion on a single processor core
int exec_cpu(solver_props *props, const char *outputs_dirname, double *progress, unsigned int modelid, int resuming){
  unsigned int i;
  CDATAFORMAT min_time;
  int inputs_available = 1;
  unsigned int iterid = NUM_ITERATORS - 1;

  // Initialize all iterators to running
  for(i=0;i<NUM_ITERATORS;i++){
    props[i].running[modelid] = 1;
    props[i].dirty_states[modelid] = 0;
    props[i].ready_outputs[modelid] = 0;
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
    exec_buffer_outputs(min_time,props,modelid,resuming);

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
    exec_update_and_postprocess(min_time,props,modelid,resuming);

    // Advance the iterator.
    solver_advance(min_time,props,modelid,resuming);

    exec_writeback(min_time,props,modelid);

    // Capture outputs for final iteration
    exec_final_outputs(props,modelid);
     
    // Cannot continue if a sampled input with halt condition has no more data
    if(!inputs_available) { break;}

    // Cannot continue if all the simulation is complete
    if (!model_running(props, modelid)) {break;}

    resuming = 1;

    // Preprocess phase: x[t] = f(x[t])
    exec_preprocess(min_time,props,modelid);

    exec_writeback(min_time,props,modelid);

    // Main solver evaluation phase, including inprocess.
    // x[t+dt] = f(x[t])
    if (SUCCESS != exec_solver_and_inprocess(min_time, props, modelid)) {
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
