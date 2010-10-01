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
