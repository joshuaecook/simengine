int exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs) {
  unsigned int modelid = 0;
  unsigned int status = SUCCESS;
  output_buffer *ob = (output_buffer*)malloc(sizeof(output_buffer));
#if NUM_OUTPUTS > 0
  output_data *od = (output_data*)malloc(NUM_MODELS*sizeof(output_data));
  unsigned int outputsize = sizeof(output_data)/sizeof(CDATAFORMAT);
#else
  void *od = NULL;
  unsigned int outputsize = 0;
#endif
  int *running = (int*)malloc(NUM_MODELS*sizeof(int));
  solver_props props;
  props.timestep = DT;
  props.abstol = ABSTOL;
  props.reltol = RELTOL;
  props.starttime = *t;
  props.stoptime = t1;
  props.time = t;
  props.model_states = model_states;
  props.inputs = inputs;
  props.outputs = (CDATAFORMAT*)od;
  props.statesize = NUM_STATES;
  props.inputsize = NUM_INPUTS;
  // This is the number of values needed to produce outputs including
  // values used in conditionals, not just the number of named outputs
  props.outputsize = outputsize;
  props.num_models = NUM_MODELS;
#ifndef TARGET_GPU
  props.ob_size = sizeof(output_buffer);
#else
  // 2 device buffers for async execution
  props.ob_size = 2*sizeof(output_buffer);
#endif
  props.ob = ob;
  props.running = running;

  // for CVODE
#ifdef CVODE_LMM
  props.cvode.lmm = CVODE_LMM;
#endif
#ifdef CVODE_ITER
  props.cvode.iter = CVODE_ITER;
#endif
#ifdef CVODE_SOLV
  props.cvode.solv = CVODE_SOLV;
  switch (CVODE_SOLV) {
  case CVODE_BAND:
#ifdef CVODE_UPPERHALFBW
#ifdef CVODE_LOWERHALFBW
    {
      int opts[] = {CVODE_UPPERHALFBW, CVODE_LOWERHALFBW};
      props.cvode.solv_opts = opts;
    }
#else
    {
      int opts[] = {1,1};
      props.cvode.solv_opts = opts;
    }
#endif
   break;       
#endif
  default:
    props.cvode.solv_opts = NULL;       
  }
#endif

  // Initialize run status flags
  for(modelid=0;modelid<NUM_MODELS;modelid++){
    ob->finished[modelid] = 0;
    running[modelid] = 1;
  }
	     
  INTEGRATION_MEM *mem = SOLVER(INTEGRATION_METHOD, init, TARGET, SIMENGINE_STORAGE, &props);

  // Execute the model(s) on the appropriate target
#if defined(TARGET_CPU)
  status = exec_serial_cpu(mem, outputs);
#elif defined(TARGET_OPENMP)
  status = exec_parallel_cpu(mem, outputs);
#elif defined(TARGET_GPU)
  status = exec_parallel_gpu(mem, &props, outputs);
#else
#error Invalid target
#endif
	     
  SOLVER(INTEGRATION_METHOD, free, TARGET, SIMENGINE_STORAGE, mem);
  free(ob);
  if(od) free(od);
  free(running);
  return status;
}
