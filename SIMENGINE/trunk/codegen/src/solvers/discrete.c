// Discrete solver for difference equations
// Copyright 2009, 2010 Simatra Modeling Technologies, L.L.C.

__HOST__
int discrete_init(solver_props *props){
  return 0;
}

__DEVICE__
int discrete_eval(solver_props *props, unsigned int modelid){
  props->next_time[modelid]+=props->timestep;

  return model_flows(props->time[modelid], props->model_states, props->next_states, props, 1, modelid);
}

__HOST__
int discrete_free(solver_props *props){
  return 0;
}
