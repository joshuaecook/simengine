// semeta_seint.h
/**********************************************************************/
const simengine_metadata semeta = {
  HASHCODE,
  NUM_MODELS,
  solver,
  target,
  sizeof(CDATAFORMAT)
};

const simengine_interface seint = {
  VERSION,
  NUM_INPUTS,
  NUM_STATES,
  NUM_OUTPUTS,
  input_names,
  state_names,
  output_names,
  default_inputs,
  default_states,
  output_num_quantities,
  model_name,
  &semeta
};

simengine_alloc se_alloc = { malloc, realloc, free };
/**********************************************************************/
