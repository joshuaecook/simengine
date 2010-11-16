__DEVICE__ void exec_buffer_outputs (CDATAFORMAT min_time, solver_props *props, unsigned int modelid, int resuming);
__DEVICE__ void exec_writeback (CDATAFORMAT min_time, solver_props *props, unsigned int modelid);
__DEVICE__ void exec_update_and_postprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid, int resuming);
__DEVICE__ void exec_preprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid);
__DEVICE__ int exec_solver_and_inprocess (CDATAFORMAT min_time, solver_props *props, unsigned int modelid);
__DEVICE__ void exec_final_outputs (solver_props *props, unsigned int modelid);


// GPU execution kernel that runs each model instance for a number of iterations or until the buffer fills
__GLOBAL__ void exec_kernel_gpu(solver_props *props, int resuming, unsigned int max_iterations){
  const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;
  
  unsigned int i, num_iterations = 0;
  CDATAFORMAT min_time;
  int inputs_available = 1;

  if (modelid >= props->num_models) {
    return;
  }

  if (!model_running(props, modelid)) {
    return;
  }

  // Initialize output buffer to store output data
  init_output_buffer(gpu_ob, modelid);

  // Run up to max iterations until the output buffer is full or the simulation is complete
  while (1) {
    // Cannot continue if all the simulation is complete
    if (!model_running(props, modelid)) {
      gpu_ob->finished[modelid] = 1;
      break;
    }

    // Stop if the maximum number of iterations has been executed
    if (num_iterations++ >= max_iterations) {
      break;
    }

    // Find the nearest next_time and catch up
    min_time = find_min_time(props, modelid);

    // Advance any sampled inputs
    inputs_available = 1;
#if NUM_SAMPLED_INPUTS > 0
    // Advance any sampled inputs
    for (i=NUM_CONSTANT_INPUTS; i<NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS; i++) {
      sampled_input_t *input = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(i)];
      inputs_available &= advance_sampled_input(input, min_time, props->modelid_offset, modelid);
    }
#endif

    // Buffer any available outputs
    exec_buffer_outputs(min_time,props,modelid,resuming);

    // Cannot continue if the output buffer is full
    if (gpu_ob->full[modelid]) {
      break;
    }

    // Update and postprocess phase: x[t+dt] = f(x[t+dt])
    // Update occurs before the first iteration and after every subsequent iteration.
    exec_update_and_postprocess(min_time,props,modelid,resuming);

    // Advance the iterator.
    solver_advance(min_time,props,modelid,resuming);

    exec_writeback(min_time,props,modelid);
     
    // Capture outputs for final iteration
    exec_final_outputs(props,modelid);

    // Cannot continue if no inputs data are buffered
    if(!inputs_available) {
      if (1 == num_iterations) {
	gpu_ob->finished[modelid] = 1;
      }
      break;
    }

    // Cannot continue if all the simulation is complete
    if (!model_running(props, modelid)) {
      gpu_ob->finished[modelid] = 1;
      break;
    }

    resuming = 1;

    // Preprocess phase: x[t] = f(x[t])
    exec_preprocess(min_time,props,modelid);

    exec_writeback(min_time,props,modelid);

    // Main solver evaluation phase, including inprocess.
    // x[t+dt] = f(x[t])
    if (SUCCESS != exec_solver_and_inprocess(min_time, props, modelid)) {
      gpu_ob->finished[modelid] = 1;
      return;
    }
  }
}
