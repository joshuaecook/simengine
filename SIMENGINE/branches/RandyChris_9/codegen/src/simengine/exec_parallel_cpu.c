#ifndef TARGET_GPU
// Run all models in parallel batches on all available processor cores
int exec_parallel_cpu(solver_props *props, simengine_output *outputs){
  int ret = SUCCESS;
  // Initialize omp with one thread per processor core
  omp_set_num_threads(omp_get_num_procs());

  // Start threads
#pragma omp parallel
  {
    int status;
    unsigned int modelid;
    unsigned int thread_num = omp_get_thread_num();
    unsigned int num_threads = omp_get_num_threads();
		 
    unsigned int models_per_thread = NUM_MODELS/num_threads;
    unsigned int extra_models = NUM_MODELS%num_threads;
		 
    // Run models in batches equal to the number of cores
    for(modelid = thread_num*models_per_thread; modelid < (thread_num+1)*models_per_thread; modelid++){
      status = exec_cpu(props, outputs, modelid);
      if(status != SUCCESS){
	ret = status;
	break;
      }
    }
    // If the number of models is not an even multiple of the number of cores
    // there will be one additional batch of models, with fewer threads than
    // the number of cores
    if(thread_num < extra_models){
      modelid = num_threads * models_per_thread + thread_num;
      status = exec_cpu(props, outputs, modelid);
      if(status != SUCCESS){
	ret = status;
      }
    }
  }// Threads implicitly joined here
  return ret;
}
#endif
