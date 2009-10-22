// Discrete solver for difference equations
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

int discrete_init(solver_props *props){
  props->mem = NULL;
  props->next_states = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  return 0;
}

int discrete_eval(solver_props *props, unsigned int modelid){
  // Check if model is still running
  props->running[modelid] = ((props->count[modelid] + 1) * props->timestep) + props->starttime > props->stoptime;
  if(!props->running[modelid])
    return 0;

  props->count[modelid]++;

  return model_flows(props->time[modelid], props->model_states, props->next_states, props, 1, modelid);
}

int discrete_free(solver_props *props){
  free(props->next_states);
  return 0;
}
