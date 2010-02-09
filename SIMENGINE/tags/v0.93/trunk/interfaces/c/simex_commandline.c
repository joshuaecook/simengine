/* simex_commandline.c
 * Command line program interface to the compilation and execution of Diesel models.
 *
 * Copyright (c) 2010 Simatra Modeling Technologies
 */

#include "simex.h"

// Command line option parsing enumeration
typedef enum {
  NO_OPTION,
  START,
  STOP,
  MODELS,
  INPUT_FILE,
  STATE_INIT_FILE,
  OUTPUT_FILE,
  CPU,
  OPENMP,
  GPU,
  FLOAT,
  DOUBLE,
  HELP,
  GNUPLOT
  // Debugging only options
#ifdef SIMEX_DEBUG
  ,DEBUG,
  EMULATE,
  PROFILE,
  NOCOMPILE
#endif
} clopts;

// Commandline options parsing structure
static const struct option long_options[] = {
  {"start", required_argument, 0, START},
  {"stop", required_argument, 0, STOP},
  {"models", required_argument, 0, MODELS},
  {"inputs", required_argument, 0, INPUT_FILE},
  {"states", required_argument, 0, STATE_INIT_FILE},
  {"outputs", required_argument, 0, OUTPUT_FILE},
  {"cpu", no_argument, 0, CPU},
  {"parallelcpu", no_argument, 0, OPENMP},
  {"gpu", no_argument, 0, GPU},
  {"float", no_argument, 0, FLOAT},
  {"single", no_argument, 0, FLOAT},
  {"double", no_argument, 0, DOUBLE},
  {"help", no_argument, 0, HELP},
  {"gnuplot", no_argument, 0, GNUPLOT},
  // Debugging only options
#ifdef SIMEX_DEBUG
  {"debug", no_argument, 0, DEBUG},
  {"emulate", no_argument, 0, EMULATE},
  {"profile", no_argument, 0, PROFILE},
  {"nocompile", no_argument, 0, NOCOMPILE},
#endif
  {0, 0, 0, 0}
};

// Print the usage help
void print_usage(){
  PRINTFE("\nusage: simex <model.dsl> [--options]\n"
	  "\nIf no options are specified, the model interface is displayed.\n"
	  "\nWhere options are:\n"
	  "\t--start <n>\tThe time to start the simulation. (Default = 0)\n"
	  "\t--stop <n>\tThe time to stop the simulation. (Default = 0)\n"
	  "\t--models <n>\tThe number of models to run. (Default = 1)\n"
	  "\t--inputs <file>\t\tThe file to load inputs from. (Default - from model)\n"
	  "\t--states <file>\t\tThe file to load state initial values. (Default - from model)\n"
	  "\t--outputs <file>\tThe file to write outputs. (Default - console)\n"
	  "\t--cpu\t\tRuns the simulation on the CPU. (Default)\n"
	  "\t--parallelcpu\tRuns the simulation in parallel on all CPUs.\n"
	  "\t--gpu\t\tRuns the simulation in parallel on the GPU.\n"
	  "\t--double\tRuns the simulation in double precision. (Default)\n"
	  "\t--single\tRuns the simulation in single precision.\n"
	  "\t--float\t\t(same as --single)\n"
	  "\t--gnuplot\tFormat data for output directly to gnuplot via '| gnuplot -persist'\n"
#ifdef SIMEX_DEBUG
	  "\t--debug\t\tEnables debugging information.\n"
	  "\t--emulate\tEnables device emulation (only meaningful with --gpu.)\n"
	  "\t--profile\tEnables profiling.\n"
	  "\t--nocompile\tSkip all compilation and just execute simulation.\n"
#endif
	  "\t--help\tShow this message.\n"
	  "\n");
}

// Hide the implementation name 'openmp' from user
#define OPTS_TARGET ((0 == strcmp(opts->target, "openmp")) ? "parallelcpu" : opts->target)

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
      print_usage();
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
    case MODELS:
      if(opts->num_models){
	ERROR(Simatra:Simex:parse_args, "Number of models can only be specified once.\n");
      }
      opts->num_models = atoi(optarg);
      if(opts->num_models < 1){
	ERROR(Simatra:Simex:parse_args, "Invalid number of models %d\n", opts->num_models);
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
    case CPU:
      if(opts->target){
	ERROR(Simatra:Simex:parse_args, "Only one target option can be specified.\n"
		"\tPreviously set '%s', attempting to set 'cpu'.\n", OPTS_TARGET);
      }
      opts->target = "cpu";
      break;
    case OPENMP:
      if(opts->target){
	ERROR(Simatra:Simex:parse_args, "Only one target option can be specified.\n"
		"\tPreviously set '%s', attempting to set 'parallelcpu'.\n", OPTS_TARGET);
      }
      opts->target = "openmp";
      break;
    case GPU:
      if(opts->target){
	ERROR(Simatra:Simex:parse_args, "Only one target option can be specified.\n"
		"\tPreviously set '%s', attempting to set 'gpu'.\n", OPTS_TARGET);
      }
      opts->target = "gpu";
      break;
    case FLOAT:
      if(opts->precision){
	ERROR(Simatra:Simex:parse_args, "Only one precision option can be specified.\n"
		"\tPreviously set '%s', attempting to set 'float/single'.", opts->precision);
      }
      opts->precision = "float";
      break;
    case DOUBLE:
      if(opts->precision){
	ERROR(Simatra:Simex:parse_args, "Only one precision option can be specified.\n"
		"\tPreviously set '%s', attempting to set 'double'.", opts->precision);
      }
      opts->precision = "double";
      break;
    case GNUPLOT:
      if(opts->gnuplot){
	ERROR(Simatra:Simex:parse_args, "Option '--gnuplot' can only be specified once.\n");
      }
      opts->gnuplot = 1;
      break;
#ifdef SIMEX_DEBUG
      // These debugging options are for internal use only, less error checking is used
      // it doesn't matter if they get set more than once
    case DEBUG:
      opts->debug = 1;
      PRINTFE("Debugging enabled\n");
      break;
    case EMULATE:
      opts->emulate = 1;
      PRINTFE("Emulation enabled\n");
      break;
    case PROFILE:
      opts->profile = 1;
      PRINTFE("Profiling enabled\n");
      break;
    case NOCOMPILE:
      opts->nocompile = 1;
      PRINTFE("Skipping all compilation and executing simulation.\n");
      break;
#endif
      // Stop execution if an invalid command line option is found.
      // Force the user to correct the error instead of ignoring options that
      // are not understood. Otherwise a typo could lead to executing a simulation
      // with undesired default options.
    default:
      ERROR(Simatra:Simex:parse_args, "Invalid argument %s\n", argv[optind]);
    }
  }

  // Check that exactly one model file is passed to simex
  if(optind+1 < argc){
    PRINTFE("\n");
    while(optind < argc)
      PRINTFE("\t'%s'\n", argv[optind++]);
    ERROR(Simatra:Simex:parse_args, "Too many model files passed to simex:\n");
  }
  if(optind == argc){
    ERROR(Simatra:Simex:parse_args, "No model file passed to simex\n");
  }

  // Check that model file exists and store it's absolute path
  if(NULL == realpath(argv[optind],  opts->model_filename)){
    ERROR(Simatra:Simex:parse_args, "model file '%s' does not exist.\n", argv[optind]);
  }

  // Ensure that the stop time is later than the start time.  If they are equal,
  // (i.e. not set, default to 0) the model file will be compiled but not executed
  if(opts->stop_time < opts->start_time){
    ERROR(Simatra:Simex:parse_args, "stop time (%f) must be greater than start time (%f)\n",
	  opts->stop_time, opts->start_time);
  }

  // Set defaults for any unset options
  if(!opts->target){
    opts->target = "cpu";
  }
  if(!opts->num_models){
    opts->num_models = 1;
  }
  if(!opts->precision){
    opts->precision = "double";
  }
  if(!opts->outputs_file){
    opts->outputs_file = stdout;
  }

  // Successful parsing of command line arguments
  return 0;
}

#ifdef SIMEX_DEBUG
// This is a debugging routine for parallel models executed with the same intial states and inputs,
// All duplicate models should produce the same results
void analyze_result(const simengine_interface *iface, simengine_result *result, unsigned int num_models){
  unsigned int modelid, outputid, sampleid, quantityid;
  simengine_output *output = result->outputs;
  unsigned int error = 0;
  
  PRINTFE("\nDEBUG: Analyzing Results...  ");

  for (modelid = 0; modelid < num_models; ++modelid){
    if (0 == modelid) { continue; }
    
    double errorNorm = 0.0;
    
    for (outputid = 0; outputid < iface->num_outputs; ++outputid){
      simengine_output *op0 =
	&output[AS_IDX(iface->num_outputs, num_models, outputid, modelid-1)];
      simengine_output *op1 = 
	&output[AS_IDX(iface->num_outputs, num_models, outputid, modelid)];
      
      if (op1->num_samples != op0->num_samples){
	// Don't terminate execution, so don't use ERROR()
	PRINTFE("DEBUG ERROR: Difference of sample count from %d to %d: %d\n",
		modelid-1, modelid, op1->num_samples - op0->num_samples);
	continue;
      }

      for (sampleid = 0; sampleid < op1->num_samples; ++sampleid){
	for (quantityid = 0; quantityid < op1->num_quantities; ++quantityid){
	  double d0 = 
	    op0->data[AS_IDX(op1->num_quantities, op1->num_samples, quantityid, sampleid)];
	  double d1 = 
	    op1->data[AS_IDX(op1->num_quantities, op1->num_samples, quantityid, sampleid)];
	  
	  double diff = d1 - d0;
	  errorNorm += diff * diff;
	}
      }
    }

    if (0.0 < fabs(errorNorm - 0.0)){
      PRINTFE("DEBUG ERROR: Difference from models %d and %d: %0.8f\n",
	      modelid-1, modelid, errorNorm);
      error = 1;
    }
  }
  if(!error){
    PRINTFE("DEBUG: Analysis found no errors.\n");
  }
}
#endif // ifdef SIMEX_DEBUG

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
  char **state_names = iface->state_names;
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
  printf("%12s : ", "Iterators");
  for(i=0;i<iface->num_iterators;i++){
    printf("%s\t", iface->iterator_names[i]);
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

// Find the simEngine executable based on location of simex (this executable)
const char *findsimEngine(const char *simex_bin){
  static char simex_path[PATH_MAX];
  static char simengine_bin[PATH_MAX];

  // Get absolute full path to this simex executable
  realpath(simex_bin, simex_path);

  // Construct the absolute full path to simEngine (same directory as simex)
  strcpy(simengine_bin, dirname(simex_path));
  strcat(simengine_bin, "/simEngine");

  return simengine_bin;
}

// Main program of simex command line
int main(int argc, char **argv){
  simengine_opts opts;

  if(argc == 1){
    // Print usage
    print_usage();
    return 0;
  }

  // Parse command line arguments
  if(parse_args(argc, argv, &opts)){
    return 1; // Command line parsing failed
  }

#ifdef SIMEX_DEBUG
  // Check if we should skip compilation
  if(opts.nocompile){
    set_names(&opts); // Need to set the name of the sim file
  }
  else
#endif
    // Compile the model
    if(runsimEngine(findsimEngine(argv[0]), &opts))
      return 1; // Compilation failed
    
  simengine_api *api = init_simulation(&opts);
  const simengine_interface *iface = api->getinterface();

  // Just print the model interface
  if(opts.stop_time == opts.start_time){
    print_interface(iface);
    return 0;
  }
  // Run the model simulation
  else{
    simengine_alloc allocator = { MALLOC, REALLOC, FREE };

    if(get_states_inputs(iface, &opts)){
      return 1;
    }

    simengine_result *result = api->runmodel(opts.start_time,
					     opts.stop_time,
					     opts.num_models,
					     opts.inputs,
					     opts.states,
					     &allocator);

    if (SUCCESS == result->status){
      write_outputs(iface, &opts, result);

      write_states(iface, &opts, result);

#ifdef SIMEX_DEBUG
      // Analyze results only when running multiple identical models
      if(!opts.inputs_file && !opts.states_file && opts.num_models > 1){
	analyze_result(iface, result, opts.num_models);
      }
#endif
    }
    else{
      WARN(Simatra:Simex:runmodel, "Simulation returned error %d: %s\n",
	      result->status, result->status_message);
    }

    FREE(opts.inputs);
    FREE(opts.states);
    // This is an ugly hack to get around nVidia not properly unloading the CUDA driver 'libcuda.so'
    // Don't free the simulation api
    //release_simulation(api);
    return 0;
  }
}
