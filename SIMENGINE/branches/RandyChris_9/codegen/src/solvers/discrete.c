// Discrete solver for difference equations
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

int discrete_init(solver_props *props){
  props->next_states = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  props->count = (unsigned int*)calloc(props->num_models,sizeof(unsigned int));
  return 0;
}

int discrete_eval(solver_props *props, unsigned int modelid){
  // Check if model is still running
  props->running[modelid] = props->time[modelid] + props->starttime + props->timestep > props->stoptime;
  if(!props->running[modelid])
    return 0;

  props->time[modelid]+=props->timestep;
  props->count[modelid]++;

  return model_flows(props->time[modelid], props->model_states, props->next_states, props, 1, modelid);
}

int discrete_free(solver_props *props){
  free(props->next_states);
  free(props->count);
  return 0;
}
