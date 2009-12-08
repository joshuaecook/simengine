int exec_loop(solver_props *props){
  Iterator iterid;
  int i;
  int status = SUCCESS;

  // Initialize solvers for all iterators
# if defined TARGET_GPU
  gpu_init();
# endif
  for(i=0;i<NUM_ITERATORS;i++){
    Iterator iterid = ITERATORS[i];
    solver_init(&props[iterid]);
  }

  // Execute the model(s) on the appropriate target
#if defined(TARGET_CPU)
  status = exec_serial_cpu(props);
#elif defined(TARGET_OPENMP)
  status = exec_parallel_cpu(props);
#elif defined(TARGET_GPU)
  status = exec_parallel_gpu(props);
#else
#error Invalid target
#endif

  // Free solvers for all iterators
  for(i=0;i<NUM_ITERATORS;i++){
    Iterator iterid = ITERATORS[i];
    solver_free(&props[iterid]);
  }
# if defined TARGET_GPU
  gpu_exit();
# endif

  return status;
}
