int exec_loop(solver_props *props){
  unsigned int iterid;
  int status = SUCCESS;

  // Initialize solvers for all iterators
  for(iterid=0;iterid<NUM_ITERATORS;iterid++){
    solver_init(&props[ITERATORS[iterid]]);
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
  for(iterid=0;iterid<NUM_ITERATORS;iterid++){
    solver_free(&props[ITERATORS[iterid]]);
  }

  return status;
}
