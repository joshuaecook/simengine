
// sort inputs with constants first, then samples, then time/value pairs and then events

#define IS_CONSTANT_INPUT(inputid) (NUM_CONSTANT_INPUTS > 0 && inputid < NUM_CONSTANT_INPUTS)
#define IS_SAMPLED_INPUT(inputid) (NUM_SAMPLED_INPUTS > 0 && inputid >= NUM_CONSTANT_INPUTS && inputid < NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS)
#define IS_TIME_VALUE_INPUT(inputid) (NUM_TIME_VALUE_INPUTS > 0 && inputid >= NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS && inputid < NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)
#define IS_EVENT_INPUT(inputid) (NUM_EVENT_INPUTS > 0 && inputid >= NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)

#define SAMPLED_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS)
#define TIME_VALUE_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS - NUM_SAMPLED_INPUTS)
#define EVENT_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS - NUM_SAMPLED_INPUTS - NUM_TIME_VALUE_INPUTS)

// Short circuit the constant case to the 
#define GET_INPUT(t, inputid, modelid) (IS_CONSTANT_INPUT(inputid) ? inputs[TARGET_IDX(NUM_INPUTS, PARALLEL_MODELS, inputid, modelid)] : get_input(t, inputid, modelid))

// TODO : SET THIS VALUE BASED ON NUMBER OF SAMPLED INPUTS AND MEMORY AVAILABLE, SPECIFICALLY FOR GPU
#define SAMPLE_BUFFER_SIZE 1024

typedef struct{
  CDATAFORMAT data[ARRAY_SIZE * SAMPLE_BUFFER_SIZE];
  CDATAFORMAT current_time[ARRAY_SIZE];
  int idx[ARRAY_SIZE];
  int buffered_size[ARRAY_SIZE];
  long file_idx[ARRAY_SIZE];
  double timestep;
  sampled_eof_option_t eof_option;
}sampled_input_t;

__DEVICE__ CDATAFORMAT constant_inputs[PARALLEL_MODELS * NUM_CONSTANT_INPUTS];
__DEVICE__ sampled_input_t sampled_inputs[STRUCT_SIZE * NUM_SAMPLED_INPUTS];

#define BYTE(val,n) ((val>>(n<<3))&0xff)

void modelid_dirname(const char *outputs_dirname, char *model_dirname, unsigned int modelid){
  int i;
  sprintf(model_dirname, "%s", outputs_dirname);
  for(i=2;i>=0;i--){
    sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(modelid, i));
  }
}

void read_constant_input(CDATAFORMAT *inputs, const char *outputs_dirname, unsigned int inputid, unsigned int modelid_offset, unsigned int modelid){
  FILE *inputfile;
  char inputfilepath[PATH_MAX];

  modelid_dirname(outputs_dirname, inputfilepath, modelid + modelid_offset);
  sprintf((inputfilepath + strlen(inputfilepath)), "/inputs/%s", seint.input_names[inputid]);

  inputfile = fopen(inputfilepath, "r");
  if(!inputfile) {
    // No file to read from, use default value
    inputs[TARGET_IDX(NUM_CONSTANT_INPUTS, PARALLEL_MODELS, inputid, modelid)] = seint.default_inputs[inputid];
  }
  else {
    // Read one double-precision value and cast it as CDATAFORMAT
    double value;
    if(1 != fread(&value, sizeof(double), 1, inputfile)){
      ERROR(Simatra:Simex:read_constant_input, "Could not read input from file '%s'.\n", inputfilepath);
    }
    inputs[TARGET_IDX(NUM_CONSTANT_INPUTS, PARALLEL_MODELS, inputid, modelid)] = value;
    fclose(inputfile);
  }
}

int read_sampled_input(sampled_input_t *input, const char *outputs_dirname, unsigned int inputid, unsigned int modelid_offset, unsigned int modelid, int skipped_samples){
  FILE *inputfile;
  char inputfilepath[PATH_MAX];
  int num_to_read;
  int num_read;
  int i;
  double value[SAMPLE_BUFFER_SIZE];

  modelid_dirname(outputs_dirname, inputfilepath, modelid + modelid_offset);
  sprintf((inputfilepath + strlen(inputfilepath)), "/inputs/%s", seint.input_names[inputid]);

  inputfile = fopen(inputfilepath, "r");
  if(!inputfile){
    if(input->file_idx[ARRAY_IDX]){
      ERROR(Simatra:Simex:read_sampled_input, "Input file '%s' could not be openend.\n", inputfilepath);
    }
    else{
      // No file to read from, use default value
      return 1;
    }
  }

  // Read data from input file
  input->file_idx[ARRAY_IDX] += skipped_samples * sizeof(double);
  if(fseek(inputfile, input->file_idx[ARRAY_IDX], SEEK_SET)){
    num_read = 0;
  }
  else{
    // Read up to SAMPLE_BUFFER_SIZE double-precision values and cast them as CDATAFORMAT
    num_read = fread(value, sizeof(double), SAMPLE_BUFFER_SIZE, inputfile);
    for (i = 0; i < num_read; i++) {
      input->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE + i] = value[i];
    }
  }
  input->file_idx[ARRAY_IDX] += num_read * sizeof(double);

  input->buffered_size[ARRAY_IDX] = num_read;

  // Handle the case when the file runs out of data
  if(feof(inputfile)){
    switch(input->eof_option){
    case SAMPLED_HALT:
      break;
    case SAMPLED_HOLD:
      if(num_read == 0){
	input->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE] = input->data[((ARRAY_IDX + 1) * SAMPLE_BUFFER_SIZE) - 1];
	input->buffered_size[ARRAY_IDX] = 1;
      }
      break;
    case SAMPLED_CYCLE:
      // Read input file in a loop until buffer is full
      input->file_idx[ARRAY_IDX] = 0;
      for(num_to_read = SAMPLE_BUFFER_SIZE - num_read; num_to_read > 0; num_to_read -= num_read){
	fseek(inputfile, 0, SEEK_SET);
	num_read = fread(value, sizeof(double), num_to_read, inputfile);
	for (i = 0; i < num_read; i++) {
	  input->data[(ARRAY_IDX+1) * SAMPLE_BUFFER_SIZE + i - num_to_read] = value[i];
	}
	input->file_idx[ARRAY_IDX] = num_read * sizeof(double);
	input->buffered_size[ARRAY_IDX] += num_read;
      }
      break;
    default:
      ERROR(Simatra:Simex:read_sampled_input, "Non-existent case of EOF Option.\n");
    }
  }

  fclose(inputfile);

  input->idx[ARRAY_IDX] = 0;

  return (input->buffered_size[ARRAY_IDX] > 0);
}

int advance_sampled_inputs(const char *outputs_dirname, CDATAFORMAT t, unsigned int modelid_offset, unsigned int modelid){
  int inputid;
  for(inputid=NUM_CONSTANT_INPUTS;inputid<NUM_CONSTANT_INPUTS+NUM_SAMPLED_INPUTS;inputid++){
    sampled_input_t *tmp = &sampled_inputs[STRUCT_IDX * NUM_INPUTS + SAMPLED_INPUT_ID(inputid)];
    int num_samples = 0;
    // If this input has an associated file
    if(tmp->file_idx[ARRAY_IDX]){
      // Compute integer number of samples to skip to
      num_samples = (int)((t - tmp->current_time[ARRAY_IDX])/ tmp->timestep);
      // Check if samples are exhausted and read more from file
      if(num_samples + tmp->idx[ARRAY_IDX] >= SAMPLE_BUFFER_SIZE){
	if(!read_sampled_input(outputs_dirname, inputid, modelid_offset, modelid, (num_samples + tmp->idx[ARRAY_IDX]) - SAMPLE_BUFFER_SIZE))
	  return 0;
      }
      // Check if input data is exhausted from file
      else if(num_samples + tmp->idx[ARRAY_IDX] >= tmp->buffered_size[ARRAY_IDX]){
	// Tell the calling function that input is exhausted
	if(tmp->eof_option == SAMPLED_HALT)
	  return 0;
	// Set the value returned to the last value buffered
	if(tmp->eof_option == SAMPLED_HOLD){
	  tmp->idx[ARRAY_IDX] = tmp->buffered_size[ARRAY_IDX] - 1;
	}
      }
      // Advance index into data buffer unless no data is in the buffer or the file was exhausted
      else{
	tmp->idx[ARRAY_IDX] += num_samples;
      }
    }
    tmp->current_time[ARRAY_IDX] += num_samples * tmp->timestep;
  }
  return 1;
}

int initialize_states(CDATAFORMAT *model_states, const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid){
  char states_path[PATH_MAX];
  FILE *states_file;
  unsigned int stateid;

  modelid_dirname(outputs_dirname, states_path, modelid + modelid_offset);
  sprintf(states_path + strlen(states_path), "/%s", "initial-states");
  states_file = fopen(states_path, "r");
  
  if(states_file){
    for(stateid=0;stateid<seint.num_states;stateid++){
      if(1 != fread(model_states + TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid), sizeof(double), 1, states_file)){
	ERROR(Simatra:Simex:initialize_states, "Could not read state '%s' for model %d from '%s'.\n", seint.state_names[stateid], modelid + modelid_offset, states_path);
      }
    }
    return 1;
  } 
  return 0;
}

void initialize_inputs(const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid, CDATAFORMAT start_time){
  CDATAFORMAT tmp_constant_inputs[PARALLEL_MODELS * NUM_CONSTANT_INPUTS];
  unsigned int inputid;

  // Initialize constant inputs
  for(inputid=0;inputid<NUM_CONSTANT_INPUTS;inputid++){
    read_constant_input(tmp_constant_inputs, outputs_dirname, inputid, modelid_offset, modelid);
  }

#ifdef TARGET_GPU
  cutilSafeCall(cudaMemcpyToSymbol(constant_inputs, tmp_constant_inputs, PARALLEL_MODELS * NUM_CONSTANT_INPUTS * sizeof(CDATAFORMAT), 0, cudaMemcpyHostToDevice));
#else
  memcpy(constant_inputs, tmp_constant_inputs, PARALLEL_MODELS * NUM_CONSTANT_INPUTS * sizeof(CDATAFORMAT));
#endif

  for(;inputid<NUM_CONSTANT_INPUTS+NUM_SAMPLED_INPUTS;inputid++){
#ifdef TARGET_GPU
    sampled_input_t stmp;
    sampled_input_t tmp = &stmp;
#else
    sampled_input_t *tmp = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];
#endif
    tmp->idx[ARRAY_IDX] = 0;
    tmp->file_idx[ARRAY_IDX] = 0;
    tmp->current_time[ARRAY_IDX] = start_time;
    tmp->timestep = seint.sampled_input_timesteps[SAMPLED_INPUT_ID(inputid)];
    tmp->eof_option = seint.sampled_input_eof_options[SAMPLED_INPUT_ID(inputid)];

    read_sampled_input(tmp, outputs_dirname, inputid, modelid_offset, modelid, (int)((start_time/tmp->timestep) + 0.5));
#ifdef TARGET_GPU
    cutilSafeCall(cudaMemcpyToSymbol(sampled_inputs, tmp, sizeof(sampled_input_t), SAMPLED_INPUT_ID(inputid) * sizeof(sampled_input_t), cudaMemcpyHostToDevice));
#endif
  }
}

__HOST__ __DEVICE__ static inline CDATAFORMAT get_sampled_input(unsigned int inputid, unsigned int modelid){
#ifdef TARGET_GPU
  // No sampled inputs on GPU
  return NAN;
#else

  sampled_input_t *tmp = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];

  // Check whether user provided an input (file_idx != 0)
  if(tmp->file_idx[ARRAY_IDX]) {
    return (CDATAFORMAT)tmp->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE + tmp->idx[ARRAY_IDX]];
  }
  else {
    // All inputs have default values 
    if(__finite(seint.default_inputs[inputid])) {
      return (CDATAFORMAT)seint.default_inputs[inputid];
    }
    else { // (may be NAN)
      ERROR(Simatra:Simex:get_sampled_input, "No value provided for input '%s'.\n", seint.input_names[inputid]);
    }
  }
#endif
}

__HOST__ __DEVICE__ static inline CDATAFORMAT get_input(unsigned int inputid, unsigned int modelid){
  assert(inputid < NUM_INPUTS);

  if(IS_CONSTANT_INPUT(inputid)) {
    return (CDATAFORMAT)constant_inputs[TARGET_IDX(NUM_INPUTS, PARALLEL_MODELS, inputid, modelid)];
  }

  if(IS_SAMPLED_INPUT(inputid)) {
    return get_sampled_input(inputid, modelid);
  }

#ifdef TARGET_GPU
  // No error reporting cabaility on the GPU
  return NAN;
#else
  if(IS_TIME_VALUE_INPUT(inputid)) {
    ERROR(Simatra:Simex:get_input, "Time/value pair inputs not yet implemented.\n");
  }

  if(IS_EVENT_INPUT(inputid)) {
    ERROR(Simatra:Simex:get_input, "Event inputs not yet implemented.\n");
  }

  ERROR(Simatra:Simex:get_input, "No such input id %d.\n", inputid);
#endif
}
