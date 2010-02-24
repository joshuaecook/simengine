
// Commandline options parsing structure
static const struct option long_options[] = {
  {"start", required_argument, 0, START},
  {"stop", required_argument, 0, STOP},
  {"seed", required_argument, 0, SEED},
  {"instances", required_argument, 0, INSTANCES},
  {"inputs", required_argument, 0, INPUT_FILE},
  {"states", required_argument, 0, STATE_INIT_FILE},
  {"outputs", required_argument, 0, OUTPUT_FILE},
  {"gnuplot", no_argument, 0, GNUPLOT},
  {"help", no_argument, 0, HELP},
  {0, 0, 0, 0}
};

#define START_SIZE 1000

// Error messages corresponding to enumerated error codes
const char *simengine_errors[] = {"Success", 
				  "Out of memory error",
				  "Flow computation error"};

/* Allocates and initializes an array of solver properties, one for each iterator. */
solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, int num_models, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs);
void free_solver_props(solver_props *props, CDATAFORMAT *model_states);
int exec_loop(solver_props *props);

// simengine_runmodel()
//
//    executes the model for the given parameters, states and simulation time
simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states){
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
	     
  // Allocate return structures
  if(seint.num_outputs){
    seresult->outputs = (simengine_output*)malloc(num_models * seint.num_outputs * sizeof(simengine_output));
  }
  else{
    seresult->outputs = NULL;
  }
  if(seint.num_states){
    seresult->final_states = (double*)malloc(num_models * seint.num_states * sizeof(double));
  }
  else{
    seresult->final_states = NULL;
  }
  seresult->final_time = (double*)malloc(num_models * sizeof(double));
  if((seint.num_outputs && !seresult->outputs) || (seint.num_states && !seresult->final_states) ||!seresult->final_time){
    seresult->status = ERRMEM;
    seresult->status_message = (char*) simengine_errors[ERRMEM];
    seresult->outputs = NULL;
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
	     
    // Initialization of output structures
    for (modelid = 0; modelid < models_per_batch; ++modelid) {
      for (outputid = 0; outputid < seint.num_outputs; ++outputid) {
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].alloc = START_SIZE;
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].num_quantities = seint.output_num_quantities[outputid];
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].num_samples = 0;
	seresult->outputs[AS_IDX(seint.num_outputs, num_models, outputid, models_executed + modelid)].data = (double*)malloc(START_SIZE*seint.output_num_quantities[outputid]*sizeof(double));
      }
    }

    // Initialize the solver properties and internal simulation memory structures
    solver_props *props = init_solver_props(start_time, stop_time, models_per_batch, parameters, model_states, &seresult->outputs[AS_IDX(seint.num_outputs, num_models, 0, models_executed)]);
    random_init(models_per_batch);
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
    case INSTANCES:
      if(opts->num_models){
	ERROR(Simatra:Simex:parse_args, "Number of model instances can only be specified once.\n");
      }
      opts->num_models = atoi(optarg);
      if(opts->num_models < 1){
	ERROR(Simatra:Simex:parse_args, "Invalid number of model instances %d\n", opts->num_models);
      }
      break;
    case INPUT_FILE:
      if(opts->inputs_file){
	ERROR(Simatra:Simex:parse_args, "Only one inputs file can be specified. '%s' OR '%s'\n", 
	      opts->inputs_filename, optarg);
      }
      opts->inputs_filename = optarg;
      opts->inputs_file = fopen(opts->inputs_filename, "r");
      if(!opts->inputs_file){
	ERROR(Simatra:Simex:parse_args, "Could not open input file '%s'.\n", opts->inputs_filename);
      }
      break;
    case STATE_INIT_FILE:
      if(opts->states_file){
	ERROR(Simatra:Simex:parse_args, "Only one states file can be specified. '%s' OR '%s'\n", 
	      opts->states_filename, optarg);
      }
      opts->states_filename = optarg;
      opts->states_file = fopen(opts->states_filename, "r");
      if(!opts->states_file){
	ERROR(Simatra:Simex:parse_args, "Could not open state initial value file '%s'.\n", opts->states_filename);
      }
      break;
    case OUTPUT_FILE:
      if(opts->outputs_file){
	ERROR(Simatra:Simex:parse_args, "Only one output file can be specified. '%s' OR '%s'\n", 
	      opts->outputs_filename, optarg);
      }
      opts->outputs_filename = optarg;
      opts->outputs_file = fopen(opts->outputs_filename, "w");
      if(!opts->outputs_file){
	ERROR(Simatra:Simex:parse_args, "Could not open output file '%s'.\n", opts->outputs_filename);
      }
      break;
    case GNUPLOT:
      if(opts->gnuplot){
	ERROR(Simatra:Simex:parse_args, "Option '--gnuplot' can only be specified once.\n");
      }
      opts->gnuplot = 1;
      break;
      // Stop execution if an invalid command line option is found.
      // Force the user to correct the error instead of ignoring options that
      // are not understood. Otherwise a typo could lead to executing a simulation
      // with undesired default options.
    default:
      ERROR(Simatra:Simex:parse_args, "Invalid argument\n");
    }
  }

  // Check that exactly one model file is passed to simex
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
  if(!opts->outputs_file){
    opts->outputs_file = stdout;
  }

  // Successful parsing of command line arguments
  return 0;
}

// Reads states and inputs from files
int get_states_inputs(const simengine_interface *iface, simengine_opts *opts){
  unsigned int modelid;

  opts->inputs = NMALLOC(opts->num_models * iface->num_inputs, double);
  // Check for an input file
  if(opts->inputs_file){
    int i;
    // Read inputs from file
    for(i=0;i<opts->num_models * iface->num_inputs; i++){
      if(1 != fscanf(opts->inputs_file, "%lf", &(opts->inputs[i]))){
	ERROR(Simatra:Simex:get_states_inputs, "failed to read input %d of %d from '%s'.\n", i+1,
	      opts->num_models * iface->num_inputs, opts->inputs_filename);
      }
    }
    fclose(opts->inputs_file);
  }
  else{
    // Copy inputs from default inputs
    for (modelid = 0; modelid < opts->num_models; ++modelid){
      memcpy(&(opts->inputs[AS_IDX(iface->num_inputs, opts->num_models, 0, modelid)]),
	     iface->default_inputs, iface->num_inputs * sizeof(double));
    }
  }
  
  opts->states = NMALLOC(opts->num_models * iface->num_states, double);
  // Check for a state initial value file
  if(opts->states_file){
    int i;
    // Read states from file
    for(i=0;i<opts->num_models * iface->num_states; i++){
      if(1 != fscanf(opts->states_file, "%lf", &(opts->states[i]))){
	ERROR(Simatra:Simex:get_states_inputs, "failed to read state %d of %d from '%s'.\n", i+1,
	       opts->num_models * iface->num_states, opts->states_filename);
      }
    }
    fclose(opts->states_file);
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

// Write the outputs of the simulation to the specified output stream
int write_outputs(const simengine_interface *iface, simengine_opts *opts, simengine_result *result){
  FILE *outfile;
  unsigned int modelid;
  unsigned int outputid;
  unsigned int sampleid;
  unsigned int quantid;

  // Write outputs to file if set, or stdout
  outfile = opts->outputs_file ? opts->outputs_file : stdout;

  unsigned int num_models = opts->num_models;
  unsigned int num_outputs = iface->num_outputs;

  // Format a command string for gnuplot if requested
  if(opts->gnuplot){
    for(modelid=0;modelid<num_models;modelid++){
      for(outputid=0;outputid<num_outputs;outputid++){
	fprintf(outfile, (modelid + outputid) == 0 ? "plot \"-\" t \"%s - %d\" w l" : 
		", \"-\" t \"%s - %d\" w l", iface->output_names[outputid], modelid);
      }
    }
    fprintf(outfile, "\n");
  }

  fprintf(outfile, "# Model : %s\n", iface->name);
  for(modelid=0;modelid<num_models;modelid++){
    fprintf(outfile, "# Model number : %d\n", modelid);
    for(outputid=0;outputid<num_outputs;outputid++){
      unsigned int num_samples = result->outputs[modelid*num_outputs+outputid].num_samples;
      fprintf(outfile, "\n\n# Output : %s\n", iface->output_names[outputid]);
      for(sampleid=0;sampleid<num_samples;sampleid++){
	unsigned int num_quantities = result->outputs[modelid*num_outputs+outputid].num_quantities;
	for(quantid=0;quantid<num_quantities;quantid++){
	  fprintf(outfile, "%-.16e\t", result->outputs[modelid*num_outputs+outputid].data[sampleid*num_quantities+quantid]);
	}
	fprintf(outfile, "\n");
      }
      // Signal gnuplot to terminate processing for this output
      if(opts->gnuplot)
	fprintf(outfile, "e\n");
    }
  }

  return 0;
}

// Write the final state space of the simulation to the specified output stream
int write_states(const simengine_interface *iface, simengine_opts *opts, simengine_result *result){
  FILE *outfile;
  unsigned int num_models = opts->num_models;
  unsigned int num_states = iface->num_states;
  const char **state_names = iface->state_names;
  double *states = result->final_states;
  unsigned int modelid;
  unsigned int stateid;

  // Write outputs to file if set, or stdout
  outfile = opts->outputs_file ? opts->outputs_file : stdout;

  for (modelid = 0; modelid < num_models; modelid++) {
    fprintf(outfile, "\n\n# Model number : %d\n", modelid);
    fprintf(outfile, "# Final states : ");
    for (stateid = 0; stateid < num_states; stateid++) {
      fprintf(outfile, stateid == 0 ? "%s" : " %s", state_names[stateid]);
    }
    fprintf(outfile, "\n# ");
    for (stateid = 0; stateid < num_states; stateid++) {
      fprintf(outfile, stateid == 0 ? "%.16e" : " %.16e", states[stateid]);
    }
    fprintf(outfile, "\n");
  }
}

// Print the interface to the simulation
void print_interface(const simengine_interface *iface){
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
    print_interface(iface);
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

    simengine_result *result = simengine_runmodel(opts.start_time,
						  opts.stop_time,
						  opts.num_models,
						  opts.inputs,
						  opts.states);

    if (SUCCESS == result->status){
      write_outputs(iface, &opts, result);

      write_states(iface, &opts, result);
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
