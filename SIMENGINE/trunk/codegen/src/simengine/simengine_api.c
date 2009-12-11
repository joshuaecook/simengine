
#define START_SIZE 1000

/* Allocates and initializes an array of solver properties, one for each iterator. */
solver_props* init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs);
void free_solver_props(solver_props *props, CDATAFORMAT *model_states);
int exec_loop(solver_props *props);

// simEngine API: simengine_getinterface()
//
//    returns the interface to the model for the currently built shared library
EXTERN_C
const simengine_interface *simengine_getinterface(){
  return &seint;
}

// simEngine API: simengine_runmodel()
//
//    executes the model for the given parameters, states and simulation time
EXTERN_C
simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states, simengine_alloc *alloc){
  CDATAFORMAT model_states[NUM_MODELS * NUM_STATES];
  CDATAFORMAT parameters[NUM_MODELS * NUM_INPUTS];
  unsigned int stateid;
  unsigned int modelid;
  unsigned int inputid;
  unsigned int outputid;
	     
  // Set up allocation functions
  if(alloc){
    se_alloc.malloc = alloc->malloc;
    se_alloc.realloc = alloc->realloc;
    se_alloc.free = alloc->free;
  }
	     
  // Create result structure
  simengine_result *seresult = (simengine_result*)se_alloc.malloc(sizeof(simengine_result));
	     
  // Couldn't allocate return structure, return NULL
  if(!seresult) return NULL;
	     
  // Check that the number of models matches
  if(num_models != semeta.num_models){
    seresult->status = ERRNUMMDL;
    seresult->status_message = (char*) simengine_errors[ERRNUMMDL];
    seresult->outputs = NULL;
    seresult->final_states = NULL;
    seresult->final_time = NULL;
    return seresult;
  }
	     
  // Allocate return structures
  if(seint.num_outputs){
    seresult->outputs = (simengine_output*)se_alloc.malloc(semeta.num_models * seint.num_outputs * sizeof(simengine_output));
  }
  else{
    seresult->outputs = NULL;
  }
  if(seint.num_states){
    seresult->final_states = (double*)se_alloc.malloc(semeta.num_models * seint.num_states * sizeof(double));
  }
  else{
    seresult->final_states = NULL;
  }
  seresult->final_time = (double*)se_alloc.malloc(semeta.num_models * sizeof(double));
  if((seint.num_outputs && !seresult->outputs) || (seint.num_states && !seresult->final_states) ||!seresult->final_time){
    seresult->status = ERRMEM;
    seresult->status_message = (char*) simengine_errors[ERRMEM];
    seresult->outputs = NULL;
    seresult->final_states = NULL;
    seresult->final_time = NULL;
    return seresult;
  }
	     
  // Copy inputs and state initial values to internal representation
  for(modelid=0; modelid<semeta.num_models; modelid++){
    for(stateid=0;stateid<seint.num_states;stateid++){
      model_states[TARGET_IDX(seint.num_states, semeta.num_models, stateid, modelid)] = states[AS_IDX(seint.num_states, semeta.num_models, stateid, modelid)];
    }
    for(inputid=0;inputid<seint.num_inputs;inputid++){
      parameters[TARGET_IDX(seint.num_inputs, semeta.num_models, inputid, modelid)] = inputs[AS_IDX(seint.num_inputs, semeta.num_models, inputid, modelid)];
    }
  }
	     
  // Initialization of output structures
  for (modelid = 0; modelid < semeta.num_models; ++modelid) {
    for (outputid = 0; outputid < seint.num_outputs; ++outputid) {
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].alloc = START_SIZE;
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].num_quantities = seint.output_num_quantities[outputid];
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].num_samples = 0;
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].data = (double*)se_alloc.malloc(START_SIZE*seint.output_num_quantities[outputid]*sizeof(double));
    }
  }

  // Initialize the solver properties and internal simulation memory structures
  solver_props *props = init_solver_props(start_time, stop_time, parameters, model_states, seresult->outputs);
  // Run the model
  seresult->status = exec_loop(props);
  seresult->status_message = (char*) simengine_errors[seresult->status];
	     
  // Copy the final time from simulation
  for(modelid=0; modelid<semeta.num_models; modelid++){
    seresult->final_time[modelid] = props->time[modelid]; // Time from the first solver
  }

  // Free all internal simulation memory and make sure that model_states has the final state values
  free_solver_props(props, model_states);

  // Copy state values back to state initial value structure
  for(modelid=0; modelid<semeta.num_models; modelid++){
    for(stateid=0;stateid<seint.num_states;stateid++){
      seresult->final_states[AS_IDX(seint.num_states, semeta.num_models, stateid, modelid)] = model_states[TARGET_IDX(seint.num_states, semeta.num_models, stateid, modelid)];
    }
  }

  return seresult;
}

// simEngine API: simengine_evalflow()
//
//    evaluates the model flows for a given state space and inputs
//    used for interfacing an external solver
EXTERN_C
int simengine_evalflow(double t, double *y, double *dydt, double *inputs) {
  // This should only ever be used when the backend is compiled in double precision
#if defined(SIMENGINE_STORAGE_double) && !defined(TARGET_GPU) && NUM_MODELS == 1
  static solver_props *props = NULL;
  if(!props){
    props = (solver_props*)malloc(sizeof(solver_props)); // Small memory leak. How do we free?
    memset(props, 0, sizeof(solver_props));
    props->inputs = inputs;
    props->iterator = 0; // The only iterator
    CDATAFORMAT *outputs = NULL;  // Should not be written to as first_iteration is 0
  }
  int first_iteration = 0;
  int modelid = 0;
  return model_flows(t, y, dydt, props, first_iteration, modelid);
#else
  return -1;
#endif
}
