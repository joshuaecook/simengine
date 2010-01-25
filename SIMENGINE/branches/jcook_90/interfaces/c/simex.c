/* simex.c
 * Interface to the compilation and execution of Diesel models.
 *
 * Copyright (c) 2010 Simatra Modeling Technologies
 */

#include "simex.h"

// Extract the name of the model from the full path DSL filename and set up the sim file
void set_names(simengine_opts *opts){
  int len;
  int i;

  // Set the name of the model
  // Find the beginning of the filename
  for(i=strlen(opts->model_filename)-1;i>0 && opts->model_filename[i-1] != '/';i--){}
  len = strlen(opts->model_filename+i)-4;
  strncpy(opts->model_name, opts->model_filename+i, len);
  opts->model_name[len] = 0;

  // Set the name of the sim file
  strcpy(opts->sim_filename, "./"); // Simulation files are created in the working dir
  strcat(opts->sim_filename, opts->model_name);
  strcat(opts->sim_filename, ".sim");
}

// String buffer size
#define BUFSIZE 10000

// Compile the DSL model to specified target
int runsimEngine (const char *simengine_bin, simengine_opts *opts)
{
  FILE *fp;
  char settings[BUFSIZE];
  char readbuffer[BUFSIZE];
  char cmdline[BUFSIZE];
  int errored = 1;

  // Make sure the names for the model and sim file are set
  set_names(opts);

  // Construct the settings to pass to simEngine
  snprintf(settings, BUFSIZE, "{target=\\\"%s\\\",precision=\\\"%s\\\",num_models=%d,debug=%s,profile=%s,emulate=%s}", 
	   opts->target, opts->precision, opts->num_models,
#ifdef SIMEX_DEBUG
	   opts->debug ? "true" : "false", 
	   opts->profile ? "true" : "false",
	   opts->emulate ? "true" : "false"
#else
	   "false", "false", "false"
#endif
	   );

  // Construct command line to launch simEngine
  // TODO: Clean this up?  Remove reliance on sh and echo?
  snprintf(cmdline, BUFSIZE, "sh -c 'echo \"print(compile2(\\\"%s\\\", %s))\" | %s -batch 2>& 1'", opts->model_filename, settings, simengine_bin);

  // We must flush because the man page says we should before a popen call
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);

  // Open the compiler
  fp = popen(cmdline, "r");
 
  if (fp == NULL){
    ERROR(Simatra:Simex:simEngine, "Could not launch simEngine compiler.\n"
	  "\tSimatra tools installation may have been corrupted.");
  }
    
  while (NULL != (fgets(readbuffer, BUFSIZE, fp))){
    if (strstr(readbuffer, "Compilation Finished Successfully") != NULL){ // <---- This is a bug waiting to happen!!!! Could be split across buffers
      errored = 0;
    }
    PRINTFE("%s", readbuffer);
  }

  PRINTFE("\n");
  pclose(fp);

  return errored;
}

// Retrieve function pointers to the simengine API calls for the simulation library
simengine_api *init_simulation(simengine_opts *opts){
  void *simengine;
  simengine_api *api;
  char *msg;
  
  if (!(simengine = dlopen(opts->sim_filename, RTLD_NOW))){
    ERROR(Simatra:Simex:dlopen, "dlopen() failed to load %s: %s", opts->sim_filename, dlerror());
  }

  api = NMALLOC(1, simengine_api);
  
  api->getinterface = (simengine_getinterface_f)dlsym(simengine, "simengine_getinterface");
  if (0 != (msg = dlerror())){
    FREE(api);
    ERROR(Simatra:Simex:dlsym, "dlsym() failed to load getinterface: %s", msg); 
  }
  api->runmodel = (simengine_runmodel_f)dlsym(simengine, "simengine_runmodel");
  if (0 != (msg = dlerror())){
    FREE(api);
    ERROR(Simatra:Simex:dlsym, "dlsym() failed to load runmodel: %s", msg); 
  }

  api->driver = simengine;
  
  return api;
}

// Releases a library handle. The given handle and associated api may no longer be used.
void release_simulation(simengine_api *api){
  dlclose(api->driver);
  FREE(api);
}
