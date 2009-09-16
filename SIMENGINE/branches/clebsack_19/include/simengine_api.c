
#define START_SIZE 1000

int exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs);

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
  CDATAFORMAT time[NUM_MODELS];
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
    seresult->status_message = simengine_errors[ERRNUMMDL];
    seresult->outputs = NULL;
    seresult->final_states = NULL;
    seresult->final_time = NULL;
    return seresult;
  }
	     
  // Allocate return structures
  seresult->outputs = (simengine_output*)se_alloc.malloc(semeta.num_models * seint.num_outputs * sizeof(simengine_output));
  seresult->final_states = (double*)se_alloc.malloc(semeta.num_models * seint.num_states * sizeof(double));
  seresult->final_time = (double*)se_alloc.malloc(semeta.num_models * sizeof(double));
  if(!seresult->outputs || !seresult->final_states ||!seresult->final_time){
    seresult->status = ERRMEM;
    seresult->status_message = simengine_errors[ERRMEM];
    seresult->outputs = NULL;
    seresult->final_states = NULL;
    seresult->final_time = NULL;
    return seresult;
  }
	     
  // Copy inputs and state initial values to internal representation
  for(modelid=0; modelid<semeta.num_models; modelid++){
    time[modelid] = start_time;
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
	     
  // Run the model
  seresult->status = exec_loop(time, stop_time, parameters, model_states, seresult->outputs);
  seresult->status_message = simengine_errors[seresult->status];
	     
  // Copy state values back to state initial value structure
  for(modelid=0; modelid<semeta.num_models; modelid++){
    seresult->final_time[modelid] = time[modelid];
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
  CDATAFORMAT *outputs = NULL;  // Should not be written to as first_iteration is 0
  int first_iteration = 0;
  int modelid = 0;

  // This should only ever be used when the backend is compiled in double precision
#if defined(SIMENGINE_STORAGE_double)
  return model_flows(t, y, dydt, inputs, outputs, first_iteration, modelid);
#else
  return -1;
#endif
}
