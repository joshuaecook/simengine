
// sort inputs with constants first, then samples, then time/value pairs and then events

#define IS_CONSTANT_INPUT(inputid) (inputid < NUM_CONSTANT_INPUTS)
#define IS_SAMPLED_INPUT(inputid) (inputid >= NUM_CONSTANT_INPUTS && inputid < NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS)
#define IS_TIME_VALUE_INPUT(inputid) (inputid >= NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS && inputid < NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)
#define IS_EVENT_INPUT(inputid) (inputid >= NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)

#define SAMPLED_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS)
#define TIME_VALUE_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS - NUM_SAMPLED_INPUTS)
#define EVENT_INPUT_ID(inputid) (inputid - NUM_CONSTANT_INPUTS - NUM_SAMPLED_INPUTS - NUM_TIME_VALUE_INPUTS)

// Short circuit the constant case to the 
#define GET_INPUT(t, inputid, modelid) (IS_CONSTANT_INPUT(inputid) ? inputs[TARGET_IDX(NUM_INPUTS, PARALLEL_MODELS, inputid, modelid)] : get_input(t, inputid, modelid))

// TODO : SET THIS VALUE BASED ON NUMBER OF SAMPLED INPUTS AND MEMORY AVAILABLE, SPECIFICALLY FOR GPU
#define SAMPLE_BUFFER_SIZE 1024

typedef struct{
  double data[ARRAY_SIZE * SAMPLE_BUFFER_SIZE];
  double current_time[ARRAY_SIZE];
  int idx[ARRAY_SIZE];
  int buffered_size[ARRAY_SIZE];
  long file_idx[ARRAY_SIZE];
  double timestep;
  sampled_eof_option_t eof_option;
}sampled_input_t;

CDATAFORMAT constant_inputs[PARALLEL_MODELS * NUM_CONSTANT_INPUTS];
sampled_input_t sampled_inputs[STRUCT_SIZE * NUM_SAMPLED_INPUTS];

#define BYTE(val,n) ((val>>(n<<3))&0xff)

void modelid_dirname(const char *outputs_dirname, char *model_dirname, unsigned int modelid){
  int i;
  sprintf(model_dirname, "%s", outputs_dirname);
  for(i=2;i>=0;i--){
    sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(modelid, i));
  }
}

void read_constant_input(const char *outputs_dirname, unsigned int inputid, unsigned int modelid_offset, unsigned int modelid){
  FILE *inputfile;
  char inputfilepath[PATH_MAX];

  modelid_dirname(outputs_dirname, inputfilepath, modelid + modelid_offset);
  sprintf((inputfilepath + strlen(inputfilepath)), "/inputs/%s", seint.input_names[inputid]);

  inputfile = fopen(inputfilepath, "r");
  if(!inputfile){
    // No file to read from, use default value
    constant_inputs[TARGET_IDX(NUM_CONSTANT_INPUTS, PARALLEL_MODELS, inputid, modelid)] = seint.default_inputs[inputid];
  }
  else{
    if(1 != fread(constant_inputs + TARGET_IDX(NUM_CONSTANT_INPUTS, PARALLEL_MODELS, inputid, modelid), sizeof(CDATAFORMAT), 1, inputfile)){
      ERROR(Simatra:Simex:read_constant_input, "Could not read input from file '%s'.\n", inputfilepath);
    }
    fclose(inputfile);
  }
}

int read_sampled_input(const char *outputs_dirname, unsigned int inputid, unsigned int modelid_offset, unsigned int modelid, int skipped_samples){
  FILE *inputfile;
  char inputfilepath[PATH_MAX];
  sampled_input_t *tmp = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];
  int num_to_read;
  int num_read;

  modelid_dirname(outputs_dirname, inputfilepath, modelid + modelid_offset);
  sprintf((inputfilepath + strlen(inputfilepath)), "/inputs/%s", seint.input_names[inputid]);

  inputfile = fopen(inputfilepath, "r");
  if(!inputfile){
    if(tmp->file_idx[ARRAY_IDX]){
      ERROR(Simatra:Simex:read_sampled_input, "Input file '%s' could not be openend.\n", inputfilepath);
    }
    else{
      // No file to read from, use default value
      return 1;
    }
  }

  // Read data from input file
  tmp->file_idx[ARRAY_IDX] += skipped_samples * sizeof(double);
  if(fseek(inputfile, tmp->file_idx[ARRAY_IDX], SEEK_SET)){
    num_read = 0;
  }
  else{
    num_read = fread(tmp->data + (ARRAY_IDX * SAMPLE_BUFFER_SIZE), sizeof(double), SAMPLE_BUFFER_SIZE, inputfile);
  }
  tmp->file_idx[ARRAY_IDX] += num_read * sizeof(double);

  tmp->buffered_size[ARRAY_IDX] = num_read;

  // Handle the case when the file runs out of data
  if(num_read != SAMPLE_BUFFER_SIZE){
    switch(tmp->eof_option){
    case SAMPLED_HALT:
      break;
    case SAMPLED_HOLD:
      if(num_read == 0){
	tmp->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE] = tmp->data[((ARRAY_IDX + 1) * SAMPLE_BUFFER_SIZE) - 1];
	tmp->buffered_size[ARRAY_IDX] = 1;
      }
      break;
    case SAMPLED_CYCLE:
      // Read input file in a loop until buffer is full
      tmp->file_idx[ARRAY_IDX] = 0;
      for(num_to_read = SAMPLE_BUFFER_SIZE - num_read; num_to_read > 0; num_to_read -= num_read){
	fseek(inputfile, 0, SEEK_SET);
	num_read = fread(tmp->data + ((ARRAY_IDX + 1) * SAMPLE_BUFFER_SIZE - num_to_read), sizeof(double), num_to_read, inputfile);
	tmp->file_idx[ARRAY_IDX] = num_read * sizeof(double);
	tmp->buffered_size[ARRAY_IDX] += num_read;
      }
      break;
    default:
      ERROR(Simatra:Simex:read_sampled_input, "Non-existent case of EOF Option.\n");
    }
  }

  fclose(inputfile);

  tmp->idx[ARRAY_IDX] = 0;

  return (tmp->buffered_size[ARRAY_IDX] > 0);
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

void initialize_states(CDATAFORMAT *model_states, const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid){
  char states_path[PATH_MAX];
  FILE *states_file;
  unsigned int stateid;

  modelid_dirname(outputs_dirname, states_path, modelid + modelid_offset);
  sprintf(states_path + strlen(states_path), "/%s", "resume_states");
  states_file = fopen(states_path, "r");
  
  if(states_file){
    for(stateid=0;stateid<seint.num_states;stateid++){
      if(1 != fread(model_states + TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid), sizeof(double), 1, states_file)){
	ERROR(Simatra:Simex:initialize_states, "Could not read state '%s' for model %d from '%s'.\n", seint.state_names[stateid], modelid + modelid_offset, states_path);
      }
    }
  } 
  else{
    // Copy default state initial values
    for(stateid=0;stateid<seint.num_states;stateid++){
      model_states[TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid)] = seint.default_states[stateid];
    }
  }
}

void initialize_inputs(const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid, CDATAFORMAT start_time){
  unsigned int inputid;

  // Initialize constant inputs
  for(inputid=0;inputid<NUM_CONSTANT_INPUTS;inputid++){
    read_constant_input(outputs_dirname, inputid, modelid_offset, modelid);
  }

  for(;inputid<NUM_CONSTANT_INPUTS+NUM_SAMPLED_INPUTS;inputid++){
    sampled_input_t *tmp = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];
    tmp->idx[ARRAY_IDX] = 0;
    tmp->file_idx[ARRAY_IDX] = 0;
    tmp->current_time[ARRAY_IDX] = start_time;
    tmp->timestep = seint.sampled_input_timesteps[SAMPLED_INPUT_ID(inputid)];
    tmp->eof_option = seint.sampled_input_eof_options[SAMPLED_INPUT_ID(inputid)];

    read_sampled_input(outputs_dirname, inputid, modelid_offset, modelid, (int)((start_time/tmp->timestep) + 0.5));
  }
}

static inline CDATAFORMAT get_sampled_input(unsigned int inputid, unsigned int modelid){
  sampled_input_t *tmp = &sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];

  // Check whether user provided an input (file_idx != 0)
  if(tmp->file_idx[ARRAY_IDX]){
    return (CDATAFORMAT)tmp->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE + tmp->idx[ARRAY_IDX]];
  }
  else{
    // All inputs have default values 
    if(__finite(seint.default_inputs[inputid]))
      return (CDATAFORMAT)seint.default_inputs[inputid];
    else // (may be NAN)
      ERROR(Simatra:Simex:get_sampled_input, "No value provided for input '%s'.\n", seint.input_names[inputid]);
  }
}

static inline CDATAFORMAT get_input(unsigned int inputid, unsigned int modelid){
  if(IS_CONSTANT_INPUT(inputid))
    return (CDATAFORMAT)constant_inputs[TARGET_IDX(NUM_INPUTS, PARALLEL_MODELS, inputid, modelid)];

#if NUM_SAMPLED_INPUTS > 0  
  if(IS_SAMPLED_INPUT(inputid))
    return get_sampled_input(inputid, modelid);
#endif

#if NUM_TIME_VALUE_INPUTS > 0
  if(IS_TIME_VALUE_INPUT)
    ERROR(Simatra:Simex:get_input, "Time/value pair inputs not yet implemented.\n");
#endif

#if NUM_EVENT_INPUTS > 0
  if(IS_EVENT_INPUT)
    ERROR(Simatra:Simex:get_input, "Event inputs not yet implemented.\n";
#endif

    ERROR(Simatra:Simex:get_input, "No such input id %d.\n", inputid);
}
