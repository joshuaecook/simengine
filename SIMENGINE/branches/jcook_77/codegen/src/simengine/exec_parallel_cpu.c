// Run all models in parallel batches on all available processor cores
int exec_parallel_cpu(solver_props *props, const char *outputs_dir, double *progress){
  int ret = SUCCESS;
  // Initialize omp thread count
  omp_set_num_threads(props->num_models);

  // Start threads
#pragma omp parallel
  {
    int status;
    unsigned int modelid = omp_get_thread_num();

    if(modelid < props->num_models){
      status = exec_cpu(props, outputs_dir, progress, modelid);
      if(status != SUCCESS){
	ret = status;
      }
    }
  }// Threads implicitly joined here
  return ret;
}
