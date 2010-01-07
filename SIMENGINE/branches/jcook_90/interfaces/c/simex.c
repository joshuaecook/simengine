#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <dlfcn.h>
#include <getopt.h>
#include <math.h>

#include "simengine_api.h"
#include "simengine_target.h"

#ifdef isfinite
#define __finite isfinite
#endif

typedef struct{
  char *model_file;
  char *model_name;
  double start_time;
  double stop_time;
  int num_models;
  FILE *inputs;
  FILE *states;
  FILE *outputs;
  char *inputs_filename;
  char *states_filename;
  char *outputs_filename;
  char *target;
  char *precision;
  int debug;
  int emulate;
  int profile;
  int nocompile;
} simengine_opts;

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
  DEBUG,
  EMULATE,
  PROFILE,
  NOCOMPILE,
  HELP
} clopts;

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
  {"debug", no_argument, 0, DEBUG},
  {"emulate", no_argument, 0, EMULATE},
  {"profile", no_argument, 0, PROFILE},
  {"nocompile", no_argument, 0, NOCOMPILE},
  {"help", no_argument, 0, HELP},
  {0, 0, 0, 0}
};


void print_usage(){
  printf("\nusage: simex <model.dsl> [--options]\n"
	 "\nIf no options are specified, the model interface is displayed.\n"
	 "\nWhere options are:\n"
	 "\t--start <n>\tThe time to start the simulation. (Default = 0)\n"
	 "\t--stop <n>\tThe time to stop the simulation. (Default = 0)\n"
	 "\t--models <n>\tThe number of models to run. (Default = 1)\n"
	 "\t--inputs <file>\t\tThe file to load inputs from. (Default - from model)\n"
	 "\t--states <file>\t\tThe file to load state initial values. (Default - from model)\n"
	 "\t--outputs <file>\t\tThe file to write outputs. (Default - console)\n"
	 "\t--cpu\t\tRuns the simulation on the CPU. (Default)\n"
	 "\t--parallelcpu\tRuns the simulation in parallel on all CPUs.\n"
	 "\t--gpu\t\tRuns the simulation in parallel on the GPU.\n"
	 "\t--double\tRuns the simulation in double precision. (Default)\n"
	 "\t--single\tRuns the simulation in single precision.\n"
	 "\t--float\t\t(same as --single)\n"
	 "\t--debug\t\tEnables debugging information.\n"
	 "\t--emulate\t\tEnables device emulation (only meaningful with --gpu.)\n"
	 "\t--profile\tEnables profiling.\n"
         "\t--nocompile\tSkip all compilation and just execute simulation.\n"
	 "\t--help\tShow this message.\n"
	 "\n");
}

// Extract the name of the model from the full path DSL filename
char *get_model_name(const char *file){
  char *model_name;
  int len;
  int i;

  // Find the beginning of the filename
  for(i=strlen(file)-1;i>0 && file[i-1] != '/';i--){}

  len = strlen(file+i)-4;
  model_name = (char*)MALLOC(len+1);
  strncpy(model_name, file+i, len);
  model_name[len] = 0;

  return model_name;
}

#define BUFSIZE 1000

int parse_args(int argc, char **argv, simengine_opts *opts){
  int arg;
  int option_index = 0;

  memset(opts, 0, sizeof(simengine_opts));

  while(1){
    arg = getopt_long(argc, argv, "", long_options, &option_index);

    if(-1 == arg)
      break;

    switch(arg){
    case HELP:
      print_usage();
      return 1;
      break;
    case START:
      if(opts->start_time){
	printf("ERROR: start time was already set.\n");
	return 1;
      }
      opts->start_time = atof(optarg);
      if(!__finite(opts->start_time)){
	printf("Error: start time is invalid %f.\n", opts->start_time);
	return 1;
      }
      //printf("Start %s -> %f\n", optarg, opts->start_time);
      break;
    case STOP:
      if(opts->stop_time){
	printf("ERROR: stop time was already set.\n");
	return 1;
      }
      opts->stop_time = atof(optarg);
      if(!__finite(opts->stop_time)){
	printf("Error: start time is invalid %f.\n", opts->stop_time);
	return 1;
      }
      //printf("Stop %s -> %f\n", optarg, opts->stop_time);
      break;
    case MODELS:
      if(opts->num_models){
	printf("ERROR: number of models was already set.\n");
	return 1;
      }
      opts->num_models = atoi(optarg);
      if(opts->num_models < 1){
	printf("ERROR: invalud number of models %d\n", opts->num_models);
	return 1;
      }
      //printf("Number of models %d\n", opts->num_models);
      break;
    case INPUT_FILE:
      opts->inputs_filename = optarg;
      opts->inputs = fopen(opts->inputs_filename, "r");
      if(!opts->inputs){
	printf("ERROR: Could not open input file '%s'.\n", opts->inputs_filename);
	return 1;
      }
      break;
    case STATE_INIT_FILE:
      opts->states_filename = optarg;
      opts->states = fopen(opts->states_filename, "r");
      if(!opts->states){
	printf("ERROR: Could not open state initial value file '%s'.\n", opts->states_filename);
	return 1;
      }
      break;
    case OUTPUT_FILE:
      opts->outputs_filename = optarg;
      opts->outputs = fopen(opts->outputs_filename, "w");
      if(!opts->outputs){
	printf("ERROR: Could not open output file '%s'.\n", opts->outputs_filename);
	return 1;
      }
      break;
    case CPU:
      if(opts->target){
	printf("ERROR: target already set\n");
	return 1;
      }
      opts->target = "cpu";
      //printf("Target CPU\n");
      break;
    case OPENMP:
      if(opts->target){
	printf("ERROR: target already set\n");
	return 1;
      }
      opts->target = "openmp";
      //printf("Target Parallel CPU\n");
      break;
    case GPU:
      if(opts->target){
	printf("ERROR: target already set\n");
	return 1;
      }
      opts->target = "gpu";
      //printf("Target GPU\n");
      break;
    case FLOAT:
      if(opts->precision){
	printf("ERROR: precision already set\n");
	return 1;
      }
      opts->precision = "float";
      //printf("Single precision\n");
      break;
    case DOUBLE:
      if(opts->precision){
	printf("ERROR: precision already set\n");
	return 1;
      }
      opts->precision = "double";
      //printf("Double precision\n");
      break;
    case DEBUG:
      opts->debug = 1;
      printf("Debugging enabled\n");
      break;
    case EMULATE:
      opts->emulate = 1;
      printf("Emulation enabled\n");
      break;
    case PROFILE:
      opts->profile = 1;
      printf("Profiling enabled\n");
      break;
    case NOCOMPILE:
      opts->nocompile = 1;
      printf("Skipping all compilation and executing simulation.\n");
      break;
    default:
      printf("ERROR: invalid argument %s\n", argv[optind]);
      return 1;
    }
  }

  if(optind+1 < argc){
    printf("Error: too many models passed to simex\n");
    while(optind < argc)
      printf("\t%s\n", argv[optind++]);
    return 1;
  }
  if(optind == argc){
    printf("Error: no model passed to simex\n");
    return 1;
  }

  opts->model_file = argv[optind];
  opts->model_name = get_model_name(opts->model_file);
  //printf("DSL model file '%s'.\n", opts->model_file);

  if(opts->stop_time < opts->start_time){
    printf("ERROR: stop time (%f) must be greater than start time (%f)\n",
	   opts->stop_time, opts->start_time);
    return 1;
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

  if(!opts->outputs){
    opts->outputs = stdout;
  }

  return 0;
}

// Length of string to strip from the end of realpath of simex to produce path for SIMENGINE environment variable
#define BIN_SIMEX_LEN (strlen("/simex"))

// Compile the DSL model to specified target
int runsimEngine (const char *simex_bin, const simengine_opts *opts)
{
  FILE *fp;
  char simex_path[PATH_MAX];
  char settings[BUFSIZE];
  char simengine[BUFSIZE];
  char readbuffer[BUFSIZE];
  char cmdline[BUFSIZE];
  int errored = 1;

  // Set up full path to simEngine
  char *simengine_path = getenv("SIMENGINE");
  // If not set, set based on path of simex (this executable)
  if(!simengine_path){
    int pathlen;
    // Get path of simex
    realpath(simex_bin, simex_path);
    // Strip off the /bin/simex
    pathlen = strnlen(simex_path, PATH_MAX);
    simex_path[pathlen-BIN_SIMEX_LEN] = 0;
    // Set the SIMENGINE variable
    setenv("SIMENGINE", simex_path, 1);
    simengine_path = simex_path;
  }

  strcpy(simengine, simengine_path);
  strcat(simengine, "/bin/simEngine");

  snprintf(settings, BUFSIZE, "{target=\\\"%s\\\",precision=\\\"%s\\\",num_models=%d,debug=%s,profile=%s,emulate=%s}", 
	   opts->target, opts->precision, opts->num_models, 
	   opts->debug ? "true" : "false", 
	   opts->profile ? "true" : "false",
	   opts->emulate ? "true" : "false");

  snprintf(cmdline, BUFSIZE, "sh -c 'echo \"print(compile2(\\\"%s\\\", %s))\" | %s -batch 2>& 1'", opts->model_file, settings, simengine);

  /* we must flush because the man page says we should before a popen call */
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);

  fp = popen(cmdline, "r");
 
  if (fp == NULL){
    PRINTF("Error launching simEngine\nDo you need to set SIMENGINE environment variable?\n");
    return 1;
  }
    
  while (NULL != (fgets(readbuffer, BUFSIZE, fp))){
    if (strstr(readbuffer, "Compilation Finished Successfully") != NULL){
      errored = 0;
    }
    PRINTF("%s", readbuffer);
  }

  PRINTF("\n");
  pclose(fp);

  return errored;
}

/* Loads the given named dynamic library file.
 * Returns an opaque handle to the library.
 */
void *load_simengine(const char *name){
  void *simengine;
  
  if (!(simengine = dlopen(name, RTLD_NOW))){
    ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	  "dlopen() failed to load %s: %s", name, dlerror());
  }

  return simengine;
}

void analyze_result(const simengine_interface *iface, simengine_result *result, unsigned int num_models){
  unsigned int modelid, outputid, sampleid, quantityid;
  simengine_output *output = result->outputs;
  unsigned int error = 0;
  
  for (modelid = 0; modelid < num_models; ++modelid){
    if (0 == modelid) { continue; }
    
    double errorNorm = 0.0;
    
    for (outputid = 0; outputid < iface->num_outputs; ++outputid){
      simengine_output *op0 = &output[AS_IDX(iface->num_outputs, num_models, outputid, modelid-1)];
      simengine_output *op1 = &output[AS_IDX(iface->num_outputs, num_models, outputid, modelid)];
      
//	    PRINTF("%d samples in model %d output %d\n", op1->num_samples, modelid, outputid);
      if (op1->num_samples != op0->num_samples){
	PRINTF("difference of sample count from %d to %d: %d\n", modelid-1, modelid, op1->num_samples - op0->num_samples);
	continue;
      }

      for (sampleid = 0; sampleid < op1->num_samples; ++sampleid){
	for (quantityid = 0; quantityid < op1->num_quantities; ++quantityid){
	  double d0 = op0->data[AS_IDX(op1->num_quantities, op1->num_samples, quantityid, sampleid)];
	  double d1 = op1->data[AS_IDX(op1->num_quantities, op1->num_samples, quantityid, sampleid)];
	  
	  double diff = d1 - d0;
	  errorNorm += diff * diff;
	}
      }
    }

    if (0.0 < fabs(errorNorm - 0.0)){
      PRINTF("Error from %d to %d: %0.8f\n", modelid-1, modelid, errorNorm);
      error = 1;
    }
  }
  if(!error){
    PRINTF("Analysis found no errors.\n");
  }
}

/* Retrieves function pointers to the simengine API calls. */
simengine_api *init_simengine(void *simengine){
  simengine_api *api;
  char *msg;
  api = NMALLOC(1, simengine_api);
  
  api->getinterface = (simengine_getinterface_f)dlsym(simengine, "simengine_getinterface");
  if (0 != (msg = dlerror())){ 
    ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	  "dlsym() failed to load getinterface: %s", msg); 
  }
  api->runmodel = (simengine_runmodel_f)dlsym(simengine, "simengine_runmodel");
  if (0 != (msg = dlerror())){ 
    ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	  "dlsym() failed to load runmodel: %s", msg); 
  }

  api->driver = simengine;
  
  return api;
}

/* Releases a library handle. The given handle and associated api may no longer be used. */
void release_simengine(simengine_api *api){
  dlclose(api->driver);
  FREE(api);
}


int get_states_inputs(const simengine_interface *iface, simengine_opts *opts, double **states, double **inputs){
  unsigned int modelid;

  *inputs = NMALLOC(opts->num_models * iface->num_inputs, double);
  // Check for an input file
  if(opts->inputs){
    int i;
    // Read inputs from file
    for(i=0;i<opts->num_models * iface->num_inputs; i++){
      if(1 != fscanf(opts->inputs, "%lf", &((*inputs)[i]))){
	printf("ERROR: failed to read input %d of %d from '%s'.\n", i+1, opts->num_models * iface->num_inputs, opts->inputs_filename);
	return 1;
      }
    }
    fclose(opts->inputs);
  }
  else{
    // Copy inputs from default inputs
    for (modelid = 0; modelid < opts->num_models; ++modelid){
      memcpy(&(*inputs)[AS_IDX(iface->num_inputs, opts->num_models, 0, modelid)], iface->default_inputs, iface->num_inputs * sizeof(double));
    }
  }
  
  *states = NMALLOC(opts->num_models * iface->num_states, double);
  // Check for a state initial value file
  if(opts->states){
    int i;
    // Read states from file
    for(i=0;i<opts->num_models * iface->num_states; i++){
      if(1 != fscanf(opts->states, "%lf", &((*states)[i]))){
	printf("ERROR: failed to read state %d of %d from '%s'.\n", i+1, opts->num_models * iface->num_states, opts->states_filename);
	return 1;
      }
    }
    fclose(opts->states);
  }
  else{
    // Copy states from default states
    for (modelid = 0; modelid < opts->num_models; ++modelid){
      memcpy(&(*states)[AS_IDX(iface->num_states, opts->num_models, 0, modelid)], iface->default_states, iface->num_states * sizeof(double));
    }
  }

  return 0;
}

int write_outputs(const simengine_interface *iface, simengine_opts *opts, simengine_result *result){
  FILE *outfile;
  unsigned int modelid;
  unsigned int outputid;
  unsigned int sampleid;
  unsigned int quantid;

  // Write outputs to file if set, or stdout
  outfile = opts->outputs ? opts->outputs : stdout;

  unsigned int num_models = opts->num_models;
  unsigned int num_outputs = iface->num_outputs;
  fprintf(outfile, "# Model : %s\n", iface->name);
  for(modelid=0;modelid<num_models;modelid++){
    fprintf(outfile, "# Model number : %d\n", modelid);
    for(outputid=0;outputid<num_outputs;outputid++){
      unsigned int num_samples = result->outputs[modelid*num_outputs+outputid].num_samples;
      fprintf(outfile, "# Output : %s\n", iface->output_names[outputid]);
      for(sampleid=0;sampleid<num_samples;sampleid++){
	unsigned int num_quantities = result->outputs[modelid*num_outputs+outputid].num_quantities;
	for(quantid=0;quantid<num_quantities;quantid++){
	  fprintf(outfile, "%-10g\t", result->outputs[modelid*num_outputs+outputid].data[sampleid*num_quantities+quantid]);
	}
	fprintf(outfile, "\n");
      }
    }
  }

  return 0;
}

int write_states(const simengine_interface *iface, simengine_opts *opts, simengine_result *result){
  FILE *outfile;
  unsigned int num_models = opts->num_models;
  unsigned int num_states = iface->num_states;
  double *states = result->final_states;
  unsigned int modelid;
  unsigned int stateid;

  // TODO Write outputs to file if set, or stdout
  outfile = stdout;

  for (modelid = 0; modelid < num_models; modelid++) {
    fprintf(outfile, "# Model number : %d\n", modelid);
    for (stateid = 0; stateid < num_states; stateid++) {
      fprintf(outfile, stateid == 0 ? "%e" : " %e", *states);
      states++;
    }
    fprintf(outfile, "\n");
  }
}


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
    printf("%g\t",iface->default_inputs[i]);
  }
  printf("\n\n%12s : ", "States");
  for(i=0;i<iface->num_states;i++){
    printf("%s\t", iface->state_names[i]);
  }
  printf("\n%12s : ", "");
  for(i=0;i<iface->num_states;i++){
    printf("%g\t", iface->default_states[i]);
  }
  printf("\n\n%12s : ", "Outputs");
  for(i=0;i<iface->num_outputs;i++){
    printf("%s[%d]\t", iface->output_names[i], iface->output_num_quantities[i]);
  }
  printf("\n\n");
}

int main(int argc, char **argv){
  simengine_opts opts;

  if(argc == 1){
    // Print usage
    print_usage();
    return 0;
  }

  // Parse command line arguments
  if(parse_args(argc, argv, &opts)){
    return 1;
  }

  simengine_api *api;
  char libraryname[256] = "./";
  if(!opts.nocompile){
    // Compile the model
    if(runsimEngine(argv[0], &opts))
      return 1;
  }
  
  // Load the API for simulation object
  strcat(libraryname, opts.model_name);
  strcat(libraryname, ".sim");
  api = init_simengine(load_simengine(libraryname));
  
  const simengine_interface *iface = api->getinterface();


  // Just print the model interface
  if(opts.stop_time == opts.start_time){
    print_interface(iface);
    return 0;
  }
  // Run the model simulation
  else{
    simengine_alloc allocator = { MALLOC, REALLOC, FREE };

    double *inputs;
    double *states;

    if(get_states_inputs(iface, &opts, &states, &inputs)){
      return 1;
    }

    simengine_result *result = api->runmodel(opts.start_time, opts.stop_time, opts.num_models, inputs, states, &allocator);

    if (SUCCESS != result->status){
      fprintf(stderr, "ERROR: runmodel returned non-zero status %d: %s\n", result->status, result->status_message);
    }

    write_outputs(iface, &opts, result);
    //    write_states(iface, &opts, result);

    // Analyze results only when running multiple identical models
    if(!opts.inputs && !opts.states && opts.num_models > 1){
      printf("\nAnalyzing Results...  ");
      analyze_result(iface, result, opts.num_models);
    }

    FREE(inputs);
    FREE(states);
    release_simengine(api);
    return 0;
  }
}
