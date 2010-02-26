
#include <sys/stat.h>
#include <sys/types.h>

// Commandline options parsing structure
static const struct option long_options[] = {
  {"start", required_argument, 0, START},
  {"stop", required_argument, 0, STOP},
  {"seed", required_argument, 0, SEED},
#ifdef TARGET_GPU
  {"gpuid", required_argument, 0, GPUID},
#endif
  {"instances", required_argument, 0, INSTANCES},
  {"instance_offset", required_argument, 0, INSTANCE_OFFSET},
  {"inputs", required_argument, 0, INPUT_FILE},
  {"states", required_argument, 0, STATE_INIT_FILE},
  {"outputs", required_argument, 0, OUTPUT_DIR},
  {"binary", no_argument, 0, BINARY},
  {"interface", no_argument, 0, INTERFACE},
  {"json-interface", no_argument, 0, JSON_INTERFACE},
  {"help", no_argument, 0, HELP},
  {0, 0, 0, 0}
};

static int binary_files = 0;
static unsigned int global_modelid_offset = 0;

#define MAX_NUM_MODELS (0x00ffffff)
#define START_SIZE 1000

// Error messages corresponding to enumerated error codes
const char *simengine_errors[] = {"Success", 
				  "Out of memory error",
				  "Flow computation error",
                                  "Could not open output file."};

/* Allocates and initializes an array of solver properties, one for each iterator. */
solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, int num_models, CDATAFORMAT *inputs, CDATAFORMAT *model_states, char *outputs_dirname, unsigned int modelid_offset);
void free_solver_props(solver_props *props, CDATAFORMAT *model_states);
int exec_loop(solver_props *props);

// simengine_runmodel()
//
//    executes the model for the given parameters, states and simulation time
simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states, char *outputs_dirname){
  CDATAFORMAT model_states[PARALLEL_MODELS * NUM_STATES];
  CDATAFORMAT parameters[PARALLEL_MODELS * NUM_INPUTS];
  unsigned int stateid;
  unsigned int modelid;
  unsigned int inputid;
  unsigned int outputid;

  int models_executed;
  int models_per_batch;

	     
  // Create result structure
  simengine_result *seresult = (simengine_result*)malloc(sizeof(simengine_result));
	     
  // Couldn't allocate return structure, return NULL
  if(!seresult) return NULL;
	     
  if(seint.num_states){
    seresult->final_states = (double*)malloc(num_models * seint.num_states * sizeof(double));
  }
  else{
    seresult->final_states = NULL;
  }
  seresult->final_time = (double*)malloc(num_models * sizeof(double));
  if((seint.num_states && !seresult->final_states) ||!seresult->final_time){
    seresult->status = ERRMEM;
    seresult->status_message = (char*) simengine_errors[ERRMEM];
    seresult->final_states = NULL;
    seresult->final_time = NULL;
    return seresult;
  }

  // Run the parallel simulation repeatedly until all requested models have been executed
  for(models_executed = 0 ; models_executed < num_models; models_executed += PARALLEL_MODELS){
    models_per_batch = MIN(num_models - models_executed, PARALLEL_MODELS);
    
    // Copy inputs and state initial values to internal representation
    for(modelid=0; modelid<models_per_batch; modelid++){
      for(stateid=0;stateid<seint.num_states;stateid++){
	model_states[TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid)] = states[AS_IDX(seint.num_states, num_models, stateid, models_executed + modelid)];
      }
      for(inputid=0;inputid<seint.num_inputs;inputid++){
	parameters[TARGET_IDX(seint.num_inputs, PARALLEL_MODELS, inputid, modelid)] = inputs[AS_IDX(seint.num_inputs, num_models, inputid, models_executed + modelid)];
      }
    }

    // Initialize the solver properties and internal simulation memory structures
    solver_props *props = init_solver_props(start_time, stop_time, models_per_batch, parameters, model_states, outputs_dirname, models_executed+global_modelid_offset);
    // Run the model
    seresult->status = exec_loop(props);
    seresult->status_message = (char*) simengine_errors[seresult->status];

    // Copy the final time from simulation
    for(modelid=0; modelid<models_per_batch; modelid++){
      seresult->final_time[models_executed + modelid] = props->time[modelid]; // Time from the first solver
    }

    // Free all internal simulation memory and make sure that model_states has the final state values
    free_solver_props(props, model_states);

    // Copy state values back to state initial value structure
    for(modelid=0; modelid<models_per_batch; modelid++){
      for(stateid=0;stateid<seint.num_states;stateid++){
	seresult->final_states[AS_IDX(seint.num_states, num_models, stateid, models_executed + modelid)] = model_states[TARGET_IDX(seint.num_states, PARALLEL_MODELS, stateid, modelid)];
      }
    }
  }

  return seresult;
}

// Print the interface to the simulation
void print_interface(){
  const simengine_interface *iface = &seint;
  unsigned int i;
  printf("\nModel : %s\n\n", iface->name);
  printf("Target : %s\tPrecision: %s\tParallel models: %d\n\n",
	 iface->target, (iface->precision == sizeof(float)) ? "float" : "double",
	 iface->parallel_models);
  printf("%12s : ", "Iterators");
  for(i=0;i<iface->num_iterators;i++){
    printf("%s\t", iface->iterator_names[i]);
  }
  printf("\n%12s : ", "Solvers");
  for(i=0;i<iface->num_iterators;i++){
    printf("%s\t", iface->solver_names[i]);
  }
  printf("\n\n%12s : ", "Inputs");
  for(i=0;i<iface->num_inputs;i++){
    printf("%s\t", iface->input_names[i]);
  }
  printf("\n%12s : ", "");
  for(i=0;i<iface->num_inputs;i++){
    printf("%.16e\t",iface->default_inputs[i]);
  }
  printf("\n\n%12s : ", "States");
  for(i=0;i<iface->num_states;i++){
    printf("%s\t", iface->state_names[i]);
  }
  printf("\n%12s : ", "");
  for(i=0;i<iface->num_states;i++){
    printf("%.16e\t", iface->default_states[i]);
  }
  printf("\n\n%12s : ", "Outputs");
  for(i=0;i<iface->num_outputs;i++){
    printf("%s[%d]\t", iface->output_names[i], iface->output_num_quantities[i]);
  }
  printf("\n\n");
}

// Parse the command line arguments into the options that are accepted by simex
int parse_args(int argc, char **argv, simengine_opts *opts){
  int arg;
  int option_index = 0;

  // Clear the memory for the options to initialize to all zeros
  memset(opts, 0, sizeof(simengine_opts));

  while(1){
    // Get an argument
    arg = getopt_long(argc, argv, "", long_options, &option_index);

    // No more arguments
    if(-1 == arg)
      break;

    switch(arg){
    case HELP:
      //print_usage();
      return 1;
      break;
    case START:
      if(opts->start_time){
	ERROR(Simatra:Simex:parse_args, "Start time can only be specified once.\n");
      }
      opts->start_time = atof(optarg);
      if(!__finite(opts->start_time)){
	ERROR(Simatra:Simex:parse_args, "Start time is invalid %f.\n", opts->start_time);
      }
      break;
    case STOP:
      if(opts->stop_time){
	ERROR(Simatra:Simex:parse_args, "Stop time can only be specified once.\n");
      }
      opts->stop_time = atof(optarg);
      if(!__finite(opts->stop_time)){
	ERROR(Simatra:Simex:parse_args, "Stop time is invalid %f.\n", opts->stop_time);
      }
      break;
    case SEED:
      if(opts->seeded){
	ERROR(Simatra:Simex:parse_args, "Random seed can only be specified once.\n");
      }
      opts->seeded = 1;
      opts->seed = atoi(optarg);
      break;
#ifdef TARGET_GPU
    case GPUID:
      if(opts->gpuid){
	ERROR(Simatra:Simex:parse_args, "GPU ID can only be specified once.\n");
      }
      opts->gpuid = 1;
      global_gpuid = atoi(optarg);
      break;
#endif
    case INSTANCES:
      if(opts->num_models){
	ERROR(Simatra:Simex:parse_args, "Number of model instances can only be specified once.\n");
      }
      opts->num_models = atoi(optarg);
      if(opts->num_models < 1){
	ERROR(Simatra:Simex:parse_args, "Invalid number of model instances %d\n", opts->num_models);
      }
      break;
    case INSTANCE_OFFSET:
      if(global_modelid_offset){
	ERROR(Simatra:Simex:parse_args, "Model instance offset can only be specified once.\n");
      }
      global_modelid_offset = atoi(optarg);
      break;
    case INPUT_FILE:
      if(opts->inputs_filename){
	ERROR(Simatra:Simex:parse_args, "Only one inputs file can be specified. '%s' OR '%s'\n", 
	      opts->inputs_filename, optarg);
      }
      opts->inputs_filename = optarg;
      break;
    case STATE_INIT_FILE:
      if(opts->states_filename){
	ERROR(Simatra:Simex:parse_args, "Only one states file can be specified. '%s' OR '%s'\n", 
	      opts->states_filename, optarg);
      }
      opts->states_filename = optarg;
      break;
    case OUTPUT_DIR:
      if(opts->outputs_dirname){
	ERROR(Simatra:Simex:parse_args, "Only one output file can be specified. '%s' OR '%s'\n", 
	      opts->outputs_dirname, optarg);
      }
      opts->outputs_dirname = optarg;
      break;
    case BINARY:
      if(binary_files){
	ERROR(Simatra:Simex:parse_args, "Option '--binary' can only be specified once.\n");
      }
      binary_files = 1;
      break;
    case INTERFACE:
      print_interface();
      exit(0);
      break;
    case JSON_INTERFACE:
      printf(json_interface);
      exit(0);
      break;
      // Stop execution if an invalid command line option is found.
      // Force the user to correct the error instead of ignoring options that
      // are not understood. Otherwise a typo could lead to executing a simulation
      // with undesired default options.
    default:
      ERROR(Simatra:Simex:parse_args, "Invalid argument\n");
    }
  }

  // Check that no invalid parameters were passed to simulation
  if(optind < argc){
    PRINTFE("\n");
    while(optind < argc)
      PRINTFE("\t'%s'\n", argv[optind++]);
    ERROR(Simatra:Simex:parse_args, "Invalid parameters passed to simex:\n");
  }

  // Ensure that the stop time is later than the start time.  If they are equal,
  // (i.e. not set, default to 0) the model interface will be returned.
  if(opts->stop_time < opts->start_time){
    ERROR(Simatra:Simex:parse_args, "stop time (%f) must be greater than start time (%f)\n",
	  opts->stop_time, opts->start_time);
  }

  if(!opts->num_models){
    opts->num_models = 1;
  }
  if(!opts->outputs_dirname){
    opts->outputs_dirname = "simex_output";
  }

  if(mkdir(opts->outputs_dirname, 0777)){
    // TODO: allow multiple processes to share the same output directory
    ERROR(Simatra:Simex:parse_args, "Could not create output directory %s.\n",
	  opts->outputs_dirname);
  }

  if(opts->num_models > MAX_NUM_MODELS){
    ERROR(Simatra:Simex:parse_args, "Number of model instances must be less than %d, requested %d.\n", MAX_NUM_MODELS, opts->num_models);
  }
  if(global_modelid_offset > MAX_NUM_MODELS){
    ERROR(Simatra:Simex:parse_args, "Model instance offset must be less than %d, requested %d.\n", MAX_NUM_MODELS, global_modelid_offset);
  }

  long long sanity_check = 0;
  sanity_check += global_modelid_offset;
  sanity_check += opts->num_models;

  if(sanity_check > MAX_NUM_MODELS){
    ERROR(Simatra:Simex:parse_args, "Number of model instances (%d) too large for requested model instance offset (%d).\n"
	  "Maximum number of models is %d.\n", opts->num_models, global_modelid_offset, MAX_NUM_MODELS);
  }
  

  // Successful parsing of command line arguments
  return 0;
}

// Reads states and inputs from files
int get_states_inputs(const simengine_interface *iface, simengine_opts *opts){
  unsigned int modelid;
  FILE *inputs_file = NULL;
  FILE *states_file = NULL;

  if(opts->inputs_filename){
    inputs_file = fopen(opts->inputs_filename, "r");
    if(!inputs_file){
      ERROR(Simatra:Simex:parse_args, "Could not open state initial value file '%s'.\n", opts->states_filename);
    }
  }

  opts->inputs = NMALLOC(opts->num_models * iface->num_inputs, double);
  // Check for an input file
  if(inputs_file){
    int i;
    // Read inputs from file
    for(i=0;i<opts->num_models * iface->num_inputs; i++){
      if(binary_files){
	if(1 != fread(&(opts->inputs[i]), sizeof(double), 1, inputs_file)){
	  ERROR(Simatra:Simex:get_states_inputs, "failed to read input %d of %d from '%s'.\n", i+1,
		opts->num_models * iface->num_inputs, opts->inputs_filename);
	}
      }
      else{
	if(1 != fscanf(inputs_file, "%lf", &(opts->inputs[i]))){
	  ERROR(Simatra:Simex:get_states_inputs, "failed to read input %d of %d from '%s'.\n", i+1,
		opts->num_models * iface->num_inputs, opts->inputs_filename);
	}
      }
    }
    fclose(inputs_file);
  }
  else{
    // Copy inputs from default inputs
    for (modelid = 0; modelid < opts->num_models; ++modelid){
      memcpy(&(opts->inputs[AS_IDX(iface->num_inputs, opts->num_models, 0, modelid)]),
	     iface->default_inputs, iface->num_inputs * sizeof(double));
    }
  }
  
  if(opts->states_filename){
    states_file = fopen(opts->states_filename, "r");
    if(!states_file){
      ERROR(Simatra:Simex:parse_args, "Could not open state initial value file '%s'.\n", opts->states_filename);
    }
  }

  opts->states = NMALLOC(opts->num_models * iface->num_states, double);
  // Check for a state initial value file
  if(states_file){
    int i;
    // Read states from file
    for(i=0;i<opts->num_models * iface->num_states; i++){
      if(binary_files){
	if(1 != fread(&(opts->states[i]), sizeof(double), 1, states_file)){
	  ERROR(Simatra:Simex:get_states_inputs, "failed to read state %d of %d from '%s'.\n", i+1,
		opts->num_models * iface->num_states, opts->states_filename);
	}
      }
      else{
	if(1 != fscanf(states_file, "%lf", &(opts->states[i]))){
	  ERROR(Simatra:Simex:get_states_inputs, "failed to read state %d of %d from '%s'.\n", i+1,
		opts->num_models * iface->num_states, opts->states_filename);
	}
      }
    }
    fclose(states_file);
  }
  else{
    // Copy states from default states
    for (modelid = 0; modelid < opts->num_models; ++modelid){
      memcpy(&(opts->states[AS_IDX(iface->num_states, opts->num_models, 0, modelid)]),
	     iface->default_states, iface->num_states * sizeof(double));
    }
  }

  return 0;
}

#define BYTE(val,n) ((val>>(n<<3))&0xff) // Also used in log_outputs

void make_model_output_directories(simengine_opts *opts){
  // Make sure a directory for the model exists
  char model_dirname[PATH_MAX];
  unsigned int modelid, full_modelid;

  for(modelid=0;modelid<opts->num_models;modelid++){
    full_modelid = modelid+global_modelid_offset;
    sprintf(model_dirname, "%s", opts->outputs_dirname);
    int i;
    for(i=2;i>=0;i--){
      sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(full_modelid, i));
      // Only need to check return value on mkdir because we created the top level directory outputs_dirname
      if(mkdir(model_dirname, 0777)){
	if(errno != EEXIST || i == 0){
	  ERROR(Simatra::Simex::make_model_output_directories, "could not create directory '%s'\n", model_dirname);
	}
      }
    }
  }
}

void write_states_time(const simengine_interface *iface, simengine_opts *opts, simengine_result *result){
  // Make sure a directory for the model exists
  char model_dirname[PATH_MAX];
  struct stat model_dir_exist;
  unsigned int modelid, stateid;

  for(modelid=0;modelid<opts->num_models;modelid++){
    unsigned int full_modelid = modelid+global_modelid_offset;
    sprintf(model_dirname, "%s", opts->outputs_dirname);
    int i;
    for(i=2;i>=0;i--){
      sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(full_modelid, i));
    }
    char states_time_filename[PATH_MAX];
    FILE *states_time_file;

    // Write final states
    sprintf(states_time_filename, "%s/%s", model_dirname, "final-states");
    states_time_file = fopen(states_time_filename, "w");
    if(NULL == states_time_file){
      ERROR(Simatra::Simex::log_outputs, "could not open file '%s'\n", states_time_filename);
    }
    if(binary_files){
      fwrite(result->final_states + modelid*iface->num_states, sizeof(double), iface->num_states, states_time_file);
    }
    else{
      for(stateid=0;stateid<iface->num_states;stateid++){
	fprintf(states_time_file, "%s%-.16e", ((stateid == 0) ? "" : "\t"), result->final_states[modelid*iface->num_states + stateid]);
      }
      fprintf(states_time_file, "\n");
    }
    fclose(states_time_file);

    // Write final time
    sprintf(states_time_filename, "%s/%s", model_dirname, "final-time");
    states_time_file = fopen(states_time_filename, "w");
    if(NULL == states_time_file){
      ERROR(Simatra::Simex::log_outputs, "could not open file '%s'\n", states_time_filename);
    }
    if(binary_files){
      fwrite(result->final_time + modelid, sizeof(double), 1, states_time_file);
    }
    else{
      fprintf(states_time_file, "%-.16e\n", result->final_time[modelid]);
    }
    fclose(states_time_file);
  }
}

// Main program of simex command line
int main(int argc, char **argv){
  simengine_opts opts;

  if(argc == 1){
    // Print usage
    //print_usage();
    return 0;
  }

  // Parse command line arguments
  if(parse_args(argc, argv, &opts)){
    return 1; // Command line parsing failed
  }
    
  const simengine_interface *iface = &seint;

  // Just print the model interface
  if(opts.stop_time == opts.start_time){
    print_interface();
    return 0;
  }
  // Run the model simulation
  else{
    if(get_states_inputs(iface, &opts)){
      return 1;
    }

    // Seed the entropy source
    if (opts.seeded) {
      seed_entropy(opts.seed);
    } else {
      seed_entropy_with_time();
    }

    make_model_output_directories(&opts);

    simengine_result *result = simengine_runmodel(opts.start_time,
						  opts.stop_time,
						  opts.num_models,
						  opts.inputs,
						  opts.states,
						  opts.outputs_dirname);

    if (SUCCESS == result->status){
      write_states_time(iface, &opts, result);
    }
    else{
      WARN(Simatra:Simex:runmodel, "Simulation returned error %d: %s\n",
	      result->status, result->status_message);
    }

    FREE(opts.inputs);
    FREE(opts.states);
    return 0;
  }
}
