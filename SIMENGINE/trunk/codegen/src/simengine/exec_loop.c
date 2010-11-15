int exec_loop(solver_props *props, const char *outputs_dirname, double *progress, int resuming){
  int i;
  int status = SUCCESS;

  // Initialize solvers for all iterators
  for(i=0;i<NUM_ITERATORS;i++){
    solver_init(&props[i]);
  }

  // Copy the state of the PRNG
  random_copy_state_to_device();

  // Execute the model(s) on the appropriate target
#if defined(TARGET_CPU)
  status = exec_cpu(props, outputs_dirname, progress, 0, resuming);
#elif defined(TARGET_OPENMP)
  status = exec_parallel_cpu(props, outputs_dirname, progress, resuming);
#elif defined(TARGET_GPU)
  status = exec_parallel_gpu(props, outputs_dirname, progress, resuming);
#else
#error Invalid target
#endif

  random_copy_state_from_device();

  // Free solvers for all iterators
  for(i=0;i<NUM_ITERATORS;i++){
    solver_free(&props[i]);
  }
# if defined TARGET_GPU
  gpu_exit();
# endif

  return status;
}

__DEVICE__
void exec_buffer_outputs (CDATAFORMAT min_time, solver_props *props, unsigned int modelid, int resuming) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if (props[i].ready_outputs[modelid]) {
#if NUM_OUTPUTS > 0
      buffer_outputs(&props[i], modelid);
#endif
      props[i].ready_outputs[modelid] = 0;
    }
    if (resuming && props[i].dirty_states[modelid] && props[i].next_time[modelid] == min_time) {
      solver_writeback(&props[i], modelid);
      props[i].dirty_states[modelid] = 0;
    }
  }
}

__DEVICE__
void exec_writeback (CDATAFORMAT min_time, solver_props *props, unsigned int modelid) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if (props[i].dirty_states[modelid] && props[i].time[modelid] == min_time) {
      solver_writeback(&props[i], modelid);
      props[i].dirty_states[modelid] = 0;
    }
  }
}

__DEVICE__
void exec_update_and_postprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid, int resuming) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if(props[i].running[modelid] && (!resuming || props[i].next_time[modelid] == min_time)){
      props[i].dirty_states[modelid] = 0 == update(&props[i], modelid);
    }	  
    if(props[i].running[modelid] && (resuming && props[i].next_time[modelid] == min_time)){
      props[i].dirty_states[modelid] |= 0 == post_process(&props[i], modelid);
    }
  }
}

__DEVICE__
void exec_preprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if(props[i].running[modelid] && props[i].time[modelid] == min_time){
      props[i].dirty_states[modelid] = 0 == pre_process(&props[i], modelid);
    }
  }
}

__DEVICE__
int exec_solver_and_inprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid) {
  int i;
  for(i=0;i<NUM_ITERATORS;i++){
    if(props[i].running[modelid] && props[i].time[modelid] == min_time){
      if(0 != solver_eval(&props[i], modelid)) {
	return ERRCOMP;
      }
      // Now next_time == time + dt
      props[i].dirty_states[modelid] = 1;
      props[i].ready_outputs[modelid] = 1;
      // Run any in-process algebraic evaluations
      in_process(&props[i], modelid);
    }
  }
  return SUCCESS;
}

__DEVICE__
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

