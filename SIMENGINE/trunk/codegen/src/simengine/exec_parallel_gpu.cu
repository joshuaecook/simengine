int exec_parallel_gpu(solver_props *props, const char *outputs_dirname, double *progress, int resuming){
  unsigned int i;
  unsigned int inputid;
  unsigned int modelid;
  unsigned int iterid = NUM_ITERATORS - 1;
  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  unsigned int active_models;
  solver_props *device_props;
#if NUM_SAMPLED_INPUTS > 0
  sampled_input_t tmp_sampled_inputs[STRUCT_SIZE * NUM_SAMPLED_INPUTS];
#endif

  num_gpu_threads = GPU_BLOCK_SIZE < props->num_models ? GPU_BLOCK_SIZE : props->num_models;
  num_gpu_blocks = (props->num_models + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;

  // Initialize all iterators to running
  active_models = 1;
  for(modelid = 0; modelid < props->num_models; modelid++){
    for(i=0;i<NUM_ITERATORS;i++){
      props[i].running[modelid] = 1;
    }
  }
  
  // Initialize GPU device memory for all solvers (returns pointer to device memory)
  device_props = gpu_init_props(props);

  while(active_models){
    // Execute models on the GPU
    
    exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(device_props, resuming);
    resuming = 1;
    // Copy data back to the host
    cutilSafeCall(cudaMemcpyFromSymbol(&global_ob[global_ob_idx[0]], gpu_ob, sizeof(output_buffer), 0, cudaMemcpyDeviceToHost));
    // Grab time from the last iterator (to be sure to skip an always iterator if it is present)
    cutilSafeCall(cudaMemcpy(props[iterid].time, props[iterid].gpu.time, props[iterid].num_models * sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
#if NUM_SAMPLED_INPUTS > 0
    cutilSafeCall(cudaMemcpyFromSymbol(tmp_sampled_inputs, sampled_inputs, STRUCT_SIZE * NUM_SAMPLED_INPUTS * sizeof(sampled_input_t), 0, cudaMemcpyDeviceToHost));
#endif

    active_models = 0;
    // Copy data to external api interface
    for(modelid = 0; modelid < props->num_models; modelid++){
      active_models |= !global_ob[global_ob_idx[modelid]].finished[modelid];
      progress[modelid] = (props[iterid].time[modelid] - props[iterid].starttime) / (props[iterid].stoptime - props[iterid].starttime);
#if NUM_SAMPLED_INPUTS > 0
      if (!global_ob[global_ob_idx[modelid]].finished[modelid]) {
	for (inputid = NUM_CONSTANT_INPUTS; inputid < NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS; inputid++) {
	  sampled_input_t *input = &tmp_sampled_inputs[STRUCT_IDX * NUM_INPUTS + SAMPLED_INPUT_ID(inputid)];
	  if (input->idx[ARRAY_IDX] >= input->buffered_size[ARRAY_IDX]) {
	    read_sampled_input(input, props->time[ARRAY_IDX], outputs_dirname, inputid, props->num_models, props->modelid_offset, modelid);
	    cutilSafeCall(cudaMemcpyToSymbol(sampled_inputs, input, sizeof(sampled_input_t), SAMPLED_INPUT_ID(inputid) * sizeof(sampled_input_t), cudaMemcpyHostToDevice));
	  }
	}
      }
#endif
      if(0 != log_outputs(outputs_dirname, props->modelid_offset, modelid)) return ERRMEM;
    }
  }

  // Copy any remaining data back from GPU
  gpu_finalize_props(props);

  return SUCCESS;
}
