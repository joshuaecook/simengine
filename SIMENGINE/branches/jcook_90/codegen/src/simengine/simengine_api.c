
#define START_SIZE 1000

// Error messages corresponding to enumerated error codes
const char *simengine_errors[] = {"Success", 
				  "Out of memory error",
				  "Flow computation error"};

/* Allocates and initializes an array of solver properties, one for each iterator. */
solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, int num_models, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs);
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
  CDATAFORMAT model_states[PARALLEL_MODELS * NUM_STATES];
  CDATAFORMAT parameters[PARALLEL_MODELS * NUM_INPUTS];
  unsigned int stateid;
  unsigned int modelid;
  unsigned int inputid;
  unsigned int outputid;

  int models_executed;
  int models_per_batch;


  // Seed the entropy source
  seed_entropy_with_time();

	     
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
	     
  // Allocate return structures
  if(seint.num_outputs){
    seresult->outputs = (simengine_output*)se_alloc.malloc(num_models * seint.num_outputs * sizeof(simengine_output));
  }
  else{
    seresult->outputs = NULL;
  }
  if(seint.num_states){
    seresult->final_states = (double*)se_alloc.malloc(num_models * seint.num_states * sizeof(double));
  }
  else{
    seresult->final_states = NULL;
  }
  seresult->final_time = (double*)se_alloc.malloc(num_models * sizeof(double));
  if((seint.num_outputs && !seresult->outputs) || (seint.num_states && !seresult->final_states) ||!seresult->final_time){
    seresult->status = ERRMEM;
    seresult->status_message = (char*) simengine_errors[ERRMEM];
    seresult->outputs = NULL;
    seresult->final_states = NULL;
    seresult->final_time = NULL;
    return seresult;
  }

  // Run the parallel simulation repeatedly until all requested models have been executed
  for(models_executed = 0 ; models_executed < num_models; models_executed += PARALLEL_MODELS){
    models_per_batch = MIN(num_models - models_executed, PARALLEL_MODELS);
    
    // Copy inputs and state initial values to internal representation
    for(modelid=0; modelid<models_per_batch; modelid++){
      for(stateid=0;stateid<seint.num_states;stateid++){
	model_states[TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid)] = states[AS_IDX(seint.num_states, num_models, stateid, models_executed + modelid)];
      }
      for(inputid=0;inputid<seint.num_inputs;inputid++){
	parameters[TARGET_IDX(seint.num_inputs, PARALLEL_MODELS, inputid, modelid)] = inputs[AS_IDX(seint.num_inputs, num_models, inputid, models_executed + modelid)];
      }
    }
	     
    // Initialization of output structures
    for (modelid = 0; modelid < models_per_batch; ++modelid) {
      for (outputid = 0; outputid < seint.num_outputs; ++outputid) {
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].alloc = START_SIZE;
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].num_quantities = seint.output_num_quantities[outputid];
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].num_samples = 0;
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].data = (double*)se_alloc.malloc(START_SIZE*seint.output_num_quantities[outputid]*sizeof(double));
      }
    }

    // Initialize the solver properties and internal simulation memory structures
    solver_props *props = init_solver_props(start_time, stop_time, models_per_batch, parameters, model_states, &seresult->outputs[AS_IDX(seint.num_outputs, num_models, 0, models_executed)]);
    // Run the model
    seresult->status = exec_loop(props);
    seresult->status_message = (char*) simengine_errors[seresult->status];
	     
    // Copy the final time from simulation
    for(modelid=0; modelid<models_per_batch; modelid++){
      seresult->final_time[models_executed + modelid] = props->time[modelid]; // Time from the first solver
    }

    // Free all internal simulation memory and make sure that model_states has the final state values
    free_solver_props(props, model_states);

    // Copy state values back to state initial value structure
    for(modelid=0; modelid<models_per_batch; modelid++){
      for(stateid=0;stateid<seint.num_states;stateid++){
	seresult->final_states[AS_IDX(seint.num_states, num_models, stateid, models_executed + modelid)] = model_states[TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid)];
      }
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
  // This should only ever be used when the backend is compiled in double precision for a single CPU
  // Matlab has no understanding of multiple simultaneous parallel models
#if defined(SIMENGINE_STORAGE_double) && defined(TARGET_CPU)
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
