// Forward Euler Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

__HOST__
int forwardeuler_init(solver_props *props){
  props->mem = NULL;
  return 0;
}

__DEVICE__
int forwardeuler_eval(solver_props *props, unsigned int modelid){
  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  props->running[modelid] = props->time[modelid] + props->timestep <= props->stoptime;
  if(!props->running[modelid])
    return 0;

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

__HOST__
int forwardeuler_free(solver_props *props){
  return 0;
}
