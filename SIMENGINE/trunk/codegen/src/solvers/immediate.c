int immediate_init(solver_props *props) {
  return 0;
}

__DEVICE__ int immediate_eval(solver_props *props, unsigned int modelid) {
  props->next_time[modelid] = props->stoptime;

  return model_flows(props->time[modelid], props->model_states, props->next_states, props, 1, modelid);
}

int immediate_free(solver_props *props) {
  return 0;
}
