// Forward Euler Integration Method
// Copyright 2009, 2010 Simatra Modeling Technologies, L.L.C.

__HOST__
int forwardeuler_init(solver_props *props){
  props->mem = NULL;
  return 0;
}

#if defined X_IR_SPIL
__DEVICE__
int forwardeuler_eval(solver_props *props, unsigned int modelid){
  int ret = model_flows(props->time[modelid], props->model_states, props->next_states, props->model_inputs, props->model_outputs, props, 1, modelid);
  CDATAFORMAT *y = (CDATAFORMAT *)props->model_states;
  CDATAFORMAT *dydt = (CDATAFORMAT *)props->next_states;

  int i;
  for(i=props->statesize-1; i>=0; i--) {
    // Store the next state internally until updated before next iteration
    dydt[STATE_IDX] = y[STATE_IDX] + props->timestep * dydt[STATE_IDX];
  }

  props->next_time[modelid] += props->timestep;

  return ret;
}
#else
__DEVICE__
int forwardeuler_eval(solver_props *props, unsigned int modelid){
  int ret = model_flows(props->time[modelid], props->model_states, props->next_states, props, 1, modelid);

  int i;
  for(i=props->statesize-1; i>=0; i--) {
    // Store the next state internally until updated before next iteration
    props->next_states[STATE_IDX] = props->model_states[STATE_IDX] +
      props->timestep * props->next_states[STATE_IDX];
  }

  props->next_time[modelid] += props->timestep;

  return ret;
}
#endif

__HOST__
int forwardeuler_free(solver_props *props){
  return 0;
}
