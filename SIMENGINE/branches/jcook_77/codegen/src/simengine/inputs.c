
// sort inputs with scalars first, then samples, then time/value pairs and then events

#define IS_SCALAR_INPUT(inputid) (inputid < NUM_SCALAR_INPUTS)
#define IS_SAMPLED_INPUT(inputid) (inputid >= NUM_SCALAR_INPUTS && inputid < NUM_SCALAR_INPUTS + NUM_SAMPLED_INPUTS)
#define IS_TIME_VALUE_INPUT(inputid) (inputid >= NUM_SCALAR_INPUTS + NUM_SAMPLED_INPUTS && inputid < NUM_SCALAR_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)
#define IS_EVENT_INPUT(inputid) (inputid >= NUM_SCALAR_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS)

#define SAMPLED_INPUT_ID(inputid) (inputid - NUM_SCALAR_INPUTS)
#define TIME_VALUE_INPUT_ID(inputid) (inputid - NUM_SCALAR_INPUTS - NUM_SAMPLED_INPUTS)
#define EVENT_INPUT_ID(inputid) (inputid - NUM_SCALAR_INPUTS - NUM_SAMPLED_INPUTS - NUM_TIME_VALUE_INPUTS)

// Short circuit the scalar case to the 
#define GET_INPUT(t, inputid, modelid) (IS_SCALAR_INPUT(inputid) ? inputs[TARGET_IDX(NUM_INPUTS, PARALLEL_MODELS, inputid, modelid)] : get_input(t, inputid, modelid))

typedef enum {
  SAMPLED_ERROR,
  SAMPLED_HOLD,
  SAMPLED_REPEAT
} sampled_eof_option;

typedef struct{
  CDATAFORMAT data[ARRAY_SIZE * SAMPLE_BUFFERSIZE];
  int idx[ARRAY_SIZE];
  long file_idx[ARRAY_SIZE];
  int current_time[ARRAY_SIZE];
  int held[ARRAY_SIZE];
  int timestep;
  sampled_eof_option_t eof_option;
}sampled_input_t;

#define BYTE(val,n) ((val>>(n<<3))&0xff)

void modelid_dirname(const char *outputs_dirname, char *model_dirname, unsigned int modelid){
  int i;
  sprintf(model_dirname, "%s", outputs_dirname);
  for(i=2;i>=0;i--){
    sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(modelid, i));
  }
}

void read_sampled_inputs(const char *outputs_dirname, unsigned int inputid, unsigned int modelid_offset, unsigned int modelid, int skipped_samples){
  FILE *inputfile;
  char inputfilepath[PATH_MAX];
  sampled_input_t *tmp = sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + inputid];

  modelid_dirname(outputs_dirname, inputfilepath, modelid + modelid_offset);
  sprintf((inputfilepath + strlen(inputfilepath)), "/inputs/%s", seint.input_names[inputid]);

  inputfile = fopen(inputfilepath, "r");
  if(!inputfile){
    if(tmp->file_idx[ARRAY_IDX]){
      ERROR(Simatra:Simex:read_sampled_inputs, "Input file '%s' could not be openend.\n", inputfilepath);
    }
    else{
      // No file to read from, use default value
      return;
    }
  }

  int num_to_read;
  int num_read;

  for(num_to_read = SAMPLE_BUFFERSIZE; num_to_read > 0; num_to_read -= num_read){
    // Skipped samples should only occur in poorly written models with a higher sampling rate on inputs than the rate of simulation,
    // or when simulation is started at a time later than 0.
    tmp->file_idx[ARRAY_IDX] += skipped_samples;
    if(fseek(inputfile, tmp->file_idx[ARRAY_IDX], SEEK_SET)){
      num_read = 0;
    }
    else{
      num_read = fread(tmp->data + (((ARRAY_IDX+1) * SAMPLE_BUFFERSIZE) - num_to_read), sizeof(CDATAFORMAT), num_to_read, inputfile);
    }
    if(num_read != num_to_read){
      switch(tmp->eof_option){
      case SAMPLED_ERROR:
	ERROR(Simatra:Simex:read_sampled_inputs, "Input data for input '%s' has been exhausted before simulation completed.", seint.input_names[inputid]);
	break;
      case SAMPLED_HOLD:
	if(num_read == 0){
	  tmp->held[ARRAY_IDX] = 1;
	  return;
	}
	int held_value = tmp->data[ARRAY_IDX * SAMPLE_BUFFERSIZE + num_read - 1];
	for(;num_read < num_to_read; num_read++){
	  tmp->data[ARRAY_IDX * SAMPLE_BUFFERSIZE + num_read] = held_value;
	}
	break;
      case SAMPLED_REPEAT:
	tmp->file_idx[ARRAY_IDX] = 0;
	break;
      default:
	ERROR(Simatra:Simex:read_sampled_inputs, "Non-existent case of EOF Option.\n");
      }
    }
    else{
      tmp->file_idx += num_read * sizeof(CDATAFORMAT);
    }
  }

  fclose(inputfile);
  tmp->idx[ARRAY_IDX] = 0;
}

void advance_sampled_inputs(const char *outputs_dirname, CDATAFORMAT *t, unsigned int modelid_offset, unsigned int modelid){
  int inputid;
  for(inputid=0;inputid<NUM_SAMPLED_INPUTS;inputid++){
    sampled_input_t tmp = sampled_inputs[STRUCT_IDX * NUM_INPUTS + inputid];
    // Compute integer number of samples to skip to
    int num_samples = (int)(((t - tmp->current_time[ARRAY_IDX])/ tmp->timestep) + 0.5);
    // Check if default value is to be used or data is exhausted and last value held
    if(!tmp->file_idx[ARRAY_IDX] || tmp->held[ARRAY_IDX]){
      tmp->current_time[ARRAY_IDX] = t;
    }
    // Check if samples are exhausted and read more from file
    else if(num_samples + tmp->idx[ARRAY_IDX] > SAMPLE_BUFFERSIZE){
      read_sampled_inputs(outputs_dirname, inputid, modelid_offset, modelid, (num_samples + tmp->idx[ARRAY_IDX]) - SAMPLE_BUFFERSIZE);
    }
    // Otherwise just move to the next sample
    else{
      tmp->idx[ARRAY_IDX] += num_samples;
      tmp->current_time[ARRAY_IDX] = t;
    }
  }
}

// BROKEN
void initialize_states(CDATAFORMAT *model_states, const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid){
  char states_path[PATH_MAX];
  FILE *states_file;

  sprintf(states_path, "%s/%s", outputs_dirname, "resume_states");
  states_file = fopen(states_path, "r");
  
  if(states_file){
    if(fseek(states_file, ((modelid + modelid_offset) * seint.num_states * sizeof(double)), SEEK_SET)){
      fseek(states_file);
    }
  } 
  else{
  }
}

void initialize_scalar_inputs(){
}

void initialize_sampled_inputs(){
}

static inline CDATAFORMAT get_sampled_input(unsigned int inputid, unsigned int modelid){
  sampled_input_t *tmp = sampled_inputs[STRUCT_IDX * NUM_SAMPLED_INPUTS + SAMPLED_INPUT_ID(inputid)];

  // Check whether user provided an input (file_idx != 0)
  if(tmp->file_idx[ARRAY_IDX]){
      return tmp->data[ARRAY_IDX * SAMPLE_BUFFER_SIZE + tmp->idx[ARRAY_IDX]];
  }
  else{
    // All inputs have default values in scalar_inputs
    if(__finite(seint.default_inputs[inputid]))
      return seint.default_inputs[inputid];
    else
      ERROR(Simatra:Simex:get_sampled_input, "No value provided for input '%s'.\n", seint.input_names[inputid]);
  }
}

static inline CDATAFORMAT get_input(CDATAFORMAT t, unsigned int inputid, unsigned int modelid){
  if(IS_SCALAR_INPUT(inputid))
    return scalar_inputs[TARGET_IDX(NUM_INPUTS, PARALLEL_MODELS, inputid, modelid)];

#if NUM_SAMPLED_INPUTS > 0  
  if(IS_SAMPLED_INPUT(inputid))
    get_sampled_input(inputid, modelid);
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
