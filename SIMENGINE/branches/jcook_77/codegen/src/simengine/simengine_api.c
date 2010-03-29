
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
  {"outputdir", required_argument, 0, OUTPUT_DIR},
  {"binary", no_argument, 0, BINARY},
  {"interface", no_argument, 0, INTERFACE},
  {"json_interface", required_argument, 0, JSON_INTERFACE},
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
solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, unsigned int num_models, CDATAFORMAT *model_states, unsigned int modelid_offset);
void free_solver_props(solver_props *props, CDATAFORMAT *model_states);
int exec_loop(solver_props *props, const char *outputs_dir, double *progress, int resuming);
int initialize_states(CDATAFORMAT *model_states, const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid);
void initialize_inputs(const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid, CDATAFORMAT start_time);
void modelid_dirname(const char *outputs_dirname, char *model_dirname, unsigned int modelid);

void open_progress_file(const char *outputs_dirname, double **progress, int *progress_fd, unsigned int num_models){
  char progress_filename[PATH_MAX];
  double tmp = 0.0;
  int i;

  sprintf(progress_filename, "%s/progress", outputs_dirname);
  *progress_fd = open(progress_filename, O_CREAT|O_RDWR, S_IRWXU);
  if(-1 == *progress_fd){
    ERROR(Simatra::Simex::Simulation, "Could not open file to store simulation progress. '%s'\n", progress_filename);
  }
  for(i=0; i<num_models; i++){
    write(*progress_fd, &tmp, sizeof(double));
  }
  *progress = (double*)mmap(NULL, num_models * sizeof(double), PROT_READ|PROT_WRITE, MAP_SHARED, *progress_fd, 0);

  if((long long int)*progress == -1){
    ERROR(Simatra::Simex::Simulation, "Could not map progress file into memory.");
  }
}

void close_progress_file(double *progress, int progress_fd, unsigned int num_models){
  munmap(progress, num_models * sizeof(double));
  close(progress_fd);
}

// simengine_runmodel()
//
//    executes the model for the given parameters, states and simulation time
simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, const char *outputs_dirname){
  CDATAFORMAT model_states[PARALLEL_MODELS * NUM_STATES];
  unsigned int stateid;
  unsigned int modelid;
  unsigned int inputid;
  unsigned int outputid;

  unsigned int models_executed;
  unsigned int models_per_batch;

  double *progress;
  int progress_fd;

  int resuming;

  open_progress_file(outputs_dirname, &progress, &progress_fd, num_models);
	     
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
    unsigned int modelid_offset = global_modelid_offset + models_executed;
    for(modelid=0; modelid<models_per_batch; modelid++){
      resuming = initialize_states(model_states, outputs_dirname, modelid_offset, modelid);
      initialize_inputs(outputs_dirname, modelid_offset, modelid, start_time);
    }

    // Initialize the solver properties and internal simulation memory structures
    solver_props *props = init_solver_props(start_time, stop_time, models_per_batch, model_states, models_executed+global_modelid_offset);
    // Run the model
    seresult->status = exec_loop(props, outputs_dirname, progress + models_executed, resuming);
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

  close_progress_file(progress, progress_fd, num_models);

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
      opts->num_models = (unsigned int)strtod(optarg, NULL); // Handles 1E3 etc.
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
      {
	FILE *json_file;
	json_file = fopen(optarg, "w");
	if(!json_file){
	  ERROR(Simatra:Simex:parse_args, "Could not open file '%s' to write json interface.\n", optarg);
	}
	fprintf(json_file, "%s", json_interface);
	exit(0);
      }
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
    opts->outputs_dirname = "simex_outputs";
  }

  if(mkdir(opts->outputs_dirname, 0777)){
    struct stat dirname_stat;
    if(stat(opts->outputs_dirname, &dirname_stat) || !(dirname_stat.st_mode&S_IFDIR)){
      // TODO: allow multiple processes to share the same output directory
      ERROR(Simatra:Simex:parse_args, "Could not create output directory %s.\n",
	    opts->outputs_dirname);
    }
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

#define BYTE(val,n) ((val>>(n<<3))&0xff) // Also used in log_outputs

// This will create model directories for inputs/outputs if they weren't created before calling this simulation
void make_model_directories(simengine_opts *opts){
  // Make sure a directory for the model exists
  char model_dirname[PATH_MAX];
  unsigned int modelid, full_modelid;

  for(modelid=0;modelid<opts->num_models;modelid++){
    full_modelid = modelid+global_modelid_offset;
    sprintf(model_dirname, "%s", opts->outputs_dirname);
    int i;
    // FIXME: This attempts to create the upper level directories for every leaf directory, should check based on modulus and only call mkdir once for each directory
    for(i=2;i>=0;i--){
      sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(full_modelid, i));
      // Only need to check return value on mkdir because we created the top level directory outputs_dirname
      if(mkdir(model_dirname, 0777)){
	if(errno != EEXIST){
	  ERROR(Simatra::Simex::make_model_directories, "Could not create intermediate directory '%s'\n", model_dirname);
	}
      }
    }
    // Create the outputs directory
    sprintf((model_dirname + strlen(model_dirname)), "/outputs");
    if(mkdir(model_dirname, 0777)){
	  ERROR(Simatra::Simex::make_model_directories, "Output directory '%s' already exists, remove manually or specify a new output directory with the --outputdir <directory name> option\n", opts->outputs_dirname);
    }
  }
}

void write_states_time(simengine_opts *opts, simengine_result *result){
  // Make sure a directory for the model exists
  char model_dirname[PATH_MAX];
  struct stat model_dir_exist;
  unsigned int modelid, stateid;

  for(modelid=0;modelid<opts->num_models;modelid++){
    unsigned int full_modelid = modelid+global_modelid_offset;
    modelid_dirname(opts->outputs_dirname, model_dirname, full_modelid);
    char states_time_filename[PATH_MAX];
    FILE *states_time_file;

    // Write final states
    sprintf(states_time_filename, "%s/%s", model_dirname, "final-states");
    states_time_file = fopen(states_time_filename, "w");
    if(NULL == states_time_file){
      ERROR(Simatra::Simex::log_outputs, "could not open file '%s'\n", states_time_filename);
    }
    if(binary_files){
      fwrite(result->final_states + modelid*seint.num_states, sizeof(double), seint.num_states, states_time_file);
    }
    else{
      for(stateid=0;stateid<seint.num_states;stateid++){
	fprintf(states_time_file, "%s%-.16e", ((stateid == 0) ? "" : "\t"), result->final_states[modelid*seint.num_states + stateid]);
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

#include<signal.h>

// Only used to trap SIGSEGV to provide user with an intelligible error message
void signal_handler(int signal){
  ERROR(Simatra::Simex::Simulation, "Simulation performed an illegal memory access and was terminated.");
}

// Main program of simex command line
int main(int argc, char **argv){
  simengine_opts opts;

  if(argc == 1){
    // Print usage
    //print_usage();
    return 0;
  }

  // Register signal handler to trap segmentation faults
  signal(SIGSEGV, signal_handler);

  // Parse command line arguments
  if(parse_args(argc, argv, &opts)){
    return 1; // Command line parsing failed
  }
    

  if(!binary_files)
    ERROR(Simatra:Simex:main, "--binary not specified and ascii data is not currently supported.\n");

  // Just print the model interface
  if(opts.stop_time == opts.start_time){
    print_interface();
    return 0;
  }
  // Run the model simulation
  else{
    // Seed the entropy source
    if (opts.seeded) {
      seed_entropy(opts.seed);
    } else {
      seed_entropy_with_time();
    }

    make_model_directories(&opts);

    simengine_result *result = simengine_runmodel(opts.start_time,
						  opts.stop_time,
						  opts.num_models,
						  opts.outputs_dirname);

    if (SUCCESS == result->status){
      write_states_time(&opts, result);
    }
    else{
      WARN(Simatra:Simex:runmodel, "Simulation returned error %d: %s\n",
	      result->status, result->status_message);
    }

    return 0;
  }
}
