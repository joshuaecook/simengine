
// sort inputs with constants first, then samples, then time/value pairs and then events

#define IS_CONSTANT_INPUT(inputid) (NUM_CONSTANT_INPUTS > 0 && inputid < NUM_CONSTANT_INPUTS)
#define IS_SAMPLED_INPUT(inputid) (NUM_SAMPLED_INPUTS > 0 && inputid >= NUM_CONSTANT_INPUTS && inputid < NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS)
#define IS_TIME_VALUE_INPUT(inputid) (NUM_TIME_VALUE_INPUTS > 0 && inputid >= NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS && inputid < NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)
#define IS_EVENT_INPUT(inputid) (NUM_EVENT_INPUTS > 0 && inputid >= NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)

#define SAMPLED_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS)
#define TIME_VALUE_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS - NUM_SAMPLED_INPUTS)
#define EVENT_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS - NUM_SAMPLED_INPUTS - NUM_TIME_VALUE_INPUTS)

// TODO : SET THIS VALUE BASED ON NUMBER OF SAMPLED INPUTS AND MEMORY AVAILABLE, SPECIFICALLY FOR GPU
#define SAMPLE_BUFFER_SIZE 64

typedef struct{
  CDATAFORMAT data[ARRAY_SIZE * SAMPLE_BUFFER_SIZE];
  CDATAFORMAT current_time[ARRAY_SIZE];
  // index offset into the data buffer
  int idx[ARRAY_SIZE];
  // number of valid data elements
  int buffered_size[ARRAY_SIZE];
  // byte offset into the file which holds input data
  long file_idx[ARRAY_SIZE];
  CDATAFORMAT timestep;
  sampled_eof_option_t eof_option;
}sampled_input_t;


typedef struct {
  int offset;
  int length;
} inputs_index_entry_t;


#if NUM_CONSTANT_INPUTS > 0
__DEVICE__ CDATAFORMAT constant_inputs[PARALLEL_MODELS * NUM_CONSTANT_INPUTS];
#endif
#if NUM_SAMPLED_INPUTS > 0
__DEVICE__ sampled_input_t sampled_inputs[STRUCT_SIZE * NUM_SAMPLED_INPUTS];
#endif

#define BYTE(val,n) ((val>>(n<<3))&0xff)

void modelid_dirname(const char *outputs_dirname, char *model_dirname, unsigned int modelid){
  int i;
  sprintf(model_dirname, "%s", outputs_dirname);
  for(i=2;i>=0;i--){
    sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(modelid, i));
  }
}

void read_constant_inputs(CDATAFORMAT *inputs, const char *outputs_dirname, unsigned int inputid, unsigned int num_models, unsigned int models_per_batch, unsigned int modelid_offset){
  unsigned int modelid;
  char inputs_path[PATH_MAX];
  int inputs_fd;

  sprintf(inputs_path, "%s/inputs/%s", outputs_dirname, seint.input_names[inputid]);
  inputs_fd = open(inputs_path, O_RDONLY);
  if (-1 != inputs_fd) {
    struct stat filestat;
    if (0 != fstat(inputs_fd, &filestat)) {
      ERROR(Simatra:Simex:read_constant_inputs, "Unable to stat inputs file.\n");
    }

    inputs_index_entry_t *inputs_data = (inputs_index_entry_t *)mmap(NULL, filestat.st_size, PROT_READ, MAP_SHARED, inputs_fd, 0);
    double *inputs_data_ptr = (double *)(inputs_data + num_models);
    inputs_index_entry_t *index;

    for (modelid = 0; modelid < models_per_batch; modelid++) {
      index = ((inputs_index_entry_t *)inputs_data) + modelid_offset + modelid;

      assert(1 == index->length);

      inputs[TARGET_IDX(NUM_CONSTANT_INPUTS, PARALLEL_MODELS, inputid, modelid)] = *(inputs_data_ptr + index->offset);
    }

    munmap(inputs_data, filestat.st_size);
    close(inputs_fd);
  }
  else {
    // 
    for (modelid = 0; modelid < models_per_batch; modelid++) {
      inputs[TARGET_IDX(NUM_CONSTANT_INPUTS, PARALLEL_MODELS, inputid, modelid)] = seint.default_inputs[inputid];
    }
  }
}


__HOST__ int read_sampled_input(sampled_input_t *input, CDATAFORMAT t, const char *outputs_dirname, unsigned int inputid, unsigned int num_models, unsigned int modelid_offset, unsigned int modelid){
  FILE *inputfile;
  char inputfilepath[PATH_MAX];
  long skipped_samples;
  size_t index_size = num_models * sizeof(inputs_index_entry_t);
  inputs_index_entry_t index;
  int num_to_read;
  int num_read;
  int i;
  double value[SAMPLE_BUFFER_SIZE];
  CDATAFORMAT held;

  sprintf(inputfilepath, "%s/inputs/%s", outputs_dirname, seint.input_names[inputid]);
  inputfile = fopen(inputfilepath, "r");

  if(!inputfile){
    if(input->file_idx[ARRAY_IDX]){
      ERROR(Simatra:Simex:read_sampled_input, "Input file '%s' could not be opened.\n", inputfilepath);
    }
    else{
      // Default value not set
      if(!__finite(seint.default_inputs[inputid]))
	USER_ERROR(Simatra:Simex:read_sampled_input, "No value set for input '%s'. Value must be set to simulate model.\n", seint.input_names[inputid]);
      // Halt condition requires that a file be provided
      if(input->eof_option == SAMPLED_HALT)
	USER_ERROR(Simatra:Simex:read_sampled_input, "No value set for input '%s'. Value must be set to simulate model.\n", seint.input_names[inputid]);
      return 0;
    }
  }

  // Find the index entry for this model
  if (0 != fseek(inputfile, (modelid + modelid_offset) * sizeof(inputs_index_entry_t), SEEK_SET)) {
    ERROR(Simatra:Simex:read_constant_input, "Could not read input '%s' for model %d from '%s'.\n", seint.input_names[inputid], modelid + modelid_offset, inputfilepath);
  }

  if (1 != fread(&index, sizeof(inputs_index_entry_t), 1, inputfile)) {
    ERROR(Simatra:Simex:read_constant_input, "Could not read input '%s' for model %d from '%s'.\n", seint.input_names[inputid], modelid + modelid_offset, inputfilepath);
  }

  // Compute the file offset of the sample corresponding to time t
  skipped_samples = ((long)((t - input->current_time[ARRAY_IDX])/ input->timestep));
  if (input->buffered_size[ARRAY_IDX] > 0) {
    skipped_samples -= input->buffered_size[ARRAY_IDX] - input->idx[ARRAY_IDX];
  }

  // Read data from input file
  input->file_idx[ARRAY_IDX] += skipped_samples;
  // Seek to the data value
  if (0 != fseek(inputfile, index_size + ((index.offset + input->file_idx[ARRAY_IDX]) * sizeof(double)), SEEK_SET)) {
    num_read = 0;
  }
  else{
    // Read up to SAMPLE_BUFFER_SIZE double-precision values and cast them as CDATAFORMAT
    num_read = fread(value, sizeof(double), SAMPLE_BUFFER_SIZE, inputfile);
    for (i = 0; i < num_read; i++) {
      // Check for +/-INF and NAN
      if(!__finite(value[i]))
	USER_ERROR(Simatra:Simex:read_sampled_input, "Invalid value '%g' in position %lu for input '%s'.\n", value[i], input->file_idx[ARRAY_IDX] + i, seint.input_names[inputid]); 
      input->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE + i] = value[i];
    }
  }
  input->file_idx[ARRAY_IDX] += num_read;

  input->buffered_size[ARRAY_IDX] = num_read;

  // Handle the case when the file runs out of data
  if(feof(inputfile)){
    switch(input->eof_option){
    case SAMPLED_HALT:
      break;

    case SAMPLED_HOLD:
      if (0 == num_read) {
	input->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE] = input->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE + input->idx[ARRAY_IDX] - 1];
	input->buffered_size[ARRAY_IDX] = 1;
      }
      break;

    case SAMPLED_CYCLE:
      // Read input file in a loop until buffer is full
      input->file_idx[ARRAY_IDX] = 0;
      for(num_to_read = SAMPLE_BUFFER_SIZE - num_read; num_to_read > 0; num_to_read -= num_read){
	fseek(inputfile, index_size + (index.offset * sizeof(double)), SEEK_SET);
	num_read = fread(value, sizeof(double), num_to_read, inputfile);
	for (i = 0; i < num_read; i++) {
	  input->data[(ARRAY_IDX+1) * SAMPLE_BUFFER_SIZE + i - num_to_read] = value[i];
	}
	input->file_idx[ARRAY_IDX] = num_read;
	input->buffered_size[ARRAY_IDX] += num_read;
      }
      break;

    default:
      ERROR(Simatra:Simex:read_sampled_input, "Non-existent case of EOF Option.\n");
    }
  }

  fclose(inputfile);

  input->idx[ARRAY_IDX] = 0;
  input->current_time[ARRAY_IDX] += skipped_samples * input->timestep;

  return (input->buffered_size[ARRAY_IDX] > 0);
}

__DEVICE__ int advance_sampled_input(sampled_input_t *input, CDATAFORMAT t, unsigned int modelid_offset, unsigned int modelid) {
  int num_samples = 0;

  // If this input has an associated file
  if(input->file_idx[ARRAY_IDX]) {
    // Compute integer number of samples to skip to
    num_samples = (int)((t - input->current_time[ARRAY_IDX]) / input->timestep);
    input->idx[ARRAY_IDX] += num_samples;

    if (SAMPLED_HOLD == input->eof_option && input->buffered_size[ARRAY_IDX] < SAMPLE_BUFFER_SIZE && input->idx[ARRAY_IDX] >= input->buffered_size[ARRAY_IDX]) {
      input->idx[ARRAY_IDX] = input->buffered_size[ARRAY_IDX] - 1;
    }
  }
  else if (input->eof_option == SAMPLED_HALT) {
    return 0;
  }
  input->current_time[ARRAY_IDX] += num_samples * input->timestep;
  return (input->idx[ARRAY_IDX] < input->buffered_size[ARRAY_IDX]);
}

int initialize_states(CDATAFORMAT *model_states, const char *outputs_dirname, unsigned int num_models, unsigned int models_per_batch, unsigned int modelid_offset) {
  char states_path[PATH_MAX];
  int states_fd;
  
  sprintf(states_path, "%s/initial-states", outputs_dirname);
  states_fd = open(states_path, O_RDONLY);


  if (-1 != states_fd) {
    double *states_data = (double *)mmap(NULL, num_models * seint.num_states * sizeof(double), PROT_READ, MAP_SHARED, states_fd, 0);
    double *states_data_ptr = states_data + (modelid_offset * seint.num_states);
    unsigned int stateid;
    unsigned int modelid;

    // Read in model_states
    for (modelid = 0; modelid < models_per_batch; modelid++) {
      for(stateid = 0; stateid < seint.num_states; stateid++){
	model_states[TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid)] = *states_data_ptr++;
      }
    }

    munmap(states_data, num_models * seint.num_states * sizeof(double));
    close(states_fd);
    return 1;
  }
  return 0;
}

void initialize_inputs(CDATAFORMAT *tmp_constant_inputs, sampled_input_t *tmp_sampled_inputs, const char *outputs_dirname, unsigned int num_models, unsigned int models_per_batch, unsigned int modelid_offset, CDATAFORMAT start_time){
  unsigned int modelid;
  unsigned int inputid = 0;

#if NUM_CONSTANT_INPUTS > 0
  // Initialize constant inputs
  for(;inputid<NUM_CONSTANT_INPUTS;inputid++){
    read_constant_inputs(tmp_constant_inputs, outputs_dirname, inputid, num_models, models_per_batch, modelid_offset);
  }
#endif // NUM_CONSTANT_INPUTS > 0

#if NUM_SAMPLED_INPUTS > 0
  for(;inputid<NUM_CONSTANT_INPUTS+NUM_SAMPLED_INPUTS;inputid++){
    for (modelid = 0; modelid < models_per_batch; modelid++) {
      sampled_input_t *tmp = &tmp_sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];

      tmp->idx[ARRAY_IDX] = 0;
      tmp->buffered_size[ARRAY_IDX] = 0;
      tmp->file_idx[ARRAY_IDX] = 0;
      tmp->current_time[ARRAY_IDX] = start_time;
      tmp->timestep = seint.sampled_input_timesteps[SAMPLED_INPUT_ID(inputid)];
      tmp->eof_option = seint.sampled_input_eof_options[SAMPLED_INPUT_ID(inputid)];

      read_sampled_input(tmp, start_time, outputs_dirname, inputid, num_models, modelid_offset, modelid);
    }
  }
#endif // NUM_SAMPLED_INPUTS > 0

}


#if NUM_SAMPLED_INPUTS > 0
__HOST__ __DEVICE__ static inline CDATAFORMAT get_sampled_input(unsigned int inputid, unsigned int modelid){
  sampled_input_t *input = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];

  assert(input->idx[ARRAY_IDX] < SAMPLE_BUFFER_SIZE);

  return (CDATAFORMAT)input->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE + input->idx[ARRAY_IDX]];
}
#endif

__HOST__ __DEVICE__ static inline CDATAFORMAT get_input(unsigned int inputid, unsigned int modelid){
  assert(inputid < NUM_INPUTS);

#if NUM_CONSTANT_INPUTS > 0
  if(IS_CONSTANT_INPUT(inputid)) {
    return (CDATAFORMAT)constant_inputs[TARGET_IDX(NUM_CONSTANT_INPUTS, PARALLEL_MODELS, inputid, modelid)];
  }
#endif

#if NUM_SAMPLED_INPUTS > 0
  if(IS_SAMPLED_INPUT(inputid)) {
    return get_sampled_input(inputid, modelid);
  }
#endif

#ifdef TARGET_GPU
  // No error reporting cabaility on the GPU
  return NAN;
#else

#if NUM_TIME_VALUE_INPUTS > 0
  if(IS_TIME_VALUE_INPUT(inputid)) {
    ERROR(Simatra:Simex:get_input, "Time/value pair inputs not yet implemented.\n");
  }
#endif

#if NUM_EVENT_INPUTS > 0
  if(IS_EVENT_INPUT(inputid)) {
    ERROR(Simatra:Simex:get_input, "Event inputs not yet implemented.\n");
  }
#endif

  ERROR(Simatra:Simex:get_input, "No such input id %d.\n", inputid);
#endif
}
