
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
  {"inputs", required_argument, 0, INPUTS},
  {"outputdir", required_argument, 0, OUTPUT_DIR},
  {"binary", no_argument, 0, BINARY},
  {"interface", no_argument, 0, INTERFACE},
  {"json_interface", required_argument, 0, JSON_INTERFACE},
  {"shared_memory", no_argument, 0, SHARED_MEMORY},
  {"buffer_count", required_argument, 0, BUFFER_COUNT},
  {"buffer_indexing", no_argument, 0, BUFFER_INDEXING},
  {"max_iterations", required_argument, 0, MAX_ITERS},
  {"gpu_block_size", required_argument, 0, GPU_BLOCK_SZ},
  // HACK BEGIN
  {"all_timesteps", required_argument, 0, ALL_TIMESTEPS},
  // HACK END
  {"help", no_argument, 0, HELP},
  {0, 0, 0, 0}
};

static int binary_files = 0;
static int simex_output_files = 1;
static int simex_buffer_indexing = 1;
static unsigned int global_modelid_offset = 0;
static unsigned int MAX_ITERATIONS = 100;
static unsigned int GPU_BLOCK_SIZE = 128;

double global_timestep = 0.0;
unsigned int global_ob_count = 2;
output_buffer *global_ob = NULL;
unsigned int *global_ob_idx = NULL;

/* Shared memory scratch space. */
#if defined TARGET_GPU
extern __SHARED__ char block_shared_scratch[];
#else
char *block_shared_scratch;
#endif

#define MAX_NUM_MODELS (0x00ffffff)
#define START_SIZE 1000

// Error messages corresponding to enumerated error codes
const char *simengine_errors[] = {"Success", 
				  "Out of memory error",
				  "Flow computation error",
                                  "Could not open output file."};

/* Allocates and initializes an array of solver properties, one for each iterator. */
solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, unsigned int num_models, CDATAFORMAT *model_states, unsigned int modelid_offset, unsigned int gridsize, unsigned int blocksize);
void free_solver_props(solver_props *props, CDATAFORMAT *model_states);
int exec_loop(solver_props *props, const char *outputs_dir, double *progress, int resuming);
void modelid_dirname(const char *outputs_dirname, char *model_dirname, unsigned int modelid);

void open_progress_file(const char *outputs_dirname, double **progress, int *progress_fd, unsigned int num_models){
  // Writes a temporary file and renames it to prevent the MATLAB client
  // from attempting to read a partial file.
  char tmp_filename[PATH_MAX];
  int tmp_fd;
  char progress_filename[PATH_MAX];
  double tmp = 0.0;
  unsigned int i;

  sprintf(tmp_filename, "%s/simulation_progress.tmp", outputs_dirname);
  sprintf(progress_filename, "%s/simulation_progress", outputs_dirname);

  tmp_fd = open(tmp_filename, O_CREAT|O_RDWR, S_IRWXU);
  if(-1 == tmp_fd){
    ERROR(Simatra::Simex::Simulation, "Could not open file to store simulation progress. '%s'", tmp_filename);
  }
  for(i=0; i<num_models; i++){
    write(tmp_fd, &tmp, sizeof(double));
  }
  close(tmp_fd);
  if (0 != rename(tmp_filename, progress_filename)) {
    ERROR(Simatra:Simex:parse_args, "Could not rename progress file '%s' to file '%s': %s.", tmp_filename, progress_filename, strerror(errno));
  }

  *progress_fd = open(progress_filename, O_RDWR, S_IRWXU);
  if(-1 == *progress_fd){
    ERROR(Simatra::Simex::Simulation, "Could not open file to store simulation progress. '%s'", progress_filename);
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

void init_output_buffers(const char *outputs_dirname, int *output_fd, unsigned int gridsize, unsigned int blocksize){
  char buffer_file[PATH_MAX];
  unsigned int i;
  output_buffer *tmp;

  if (simex_buffer_indexing) {
    indexed_output_buffer *tmp = alloc_indexed_output_buffer(gridsize,blocksize);
    if(!tmp){
      ERROR(Simatra::Simex::Simulation, "Out of memory.\n");
    }
    sprintf(buffer_file, "%s/indexed_output_buffer", outputs_dirname);
    *output_fd = open(buffer_file, O_CREAT|O_RDWR, S_IRWXU);
    if(-1 == *output_fd){
      ERROR(Simatra::Simex::Simulation, "Could not open file to store simulation data. '%s'\n", buffer_file);
    }
    for (i=0; i<gridsize; i++) {
      write(*output_fd, &tmp[gridsize], sizeof(output_buffer));
    }
    free(tmp); // Don't need it anymore, use the mmaped version below
    global_ixob = (indexed_output_buffer *)mmap(NULL, gridsize*sizeof(indexed_output_buffer), PROT_READ|PROT_WRITE, MAP_SHARED, *output_fd, 0);

    block_shared_scratch = (char *)malloc(gridsize*blocksize*sizeof(int));
    if(!block_shared_scratch){
      ERROR(Simatra::Simex::Simulation, "Out of memory.\n");
    }
  }

  // Output buffering ca. 2010
  tmp = (output_buffer*)malloc(sizeof(output_buffer));
  if(!tmp){
    ERROR(Simatra::Simex::Simulation, "Out of memory.\n");
  }

  bzero(tmp, sizeof(output_buffer));

  if(simex_output_files){
    global_ob = tmp;
  }
  else{
    sprintf(buffer_file, "%s/output_buffer", outputs_dirname);
    *output_fd = open(buffer_file, O_CREAT|O_RDWR, S_IRWXU);
    if(-1 == *output_fd){
      ERROR(Simatra::Simex::Simulation, "Could not open file to store simulation data. '%s'\n", buffer_file);
    }
    for(i=0; i<global_ob_count; i++){
      write(*output_fd, tmp, sizeof(output_buffer));
    }
    free(tmp); // Don't need it anymore, use the mmaped version below
    global_ob = (output_buffer*)mmap(NULL, global_ob_count*sizeof(output_buffer), PROT_READ|PROT_WRITE, MAP_SHARED, *output_fd, 0);
  }
  global_ob_idx = (unsigned int*)malloc(PARALLEL_MODELS*sizeof(unsigned int));
  if(!global_ob_idx){
    ERROR(Simatra::Simex::Simulation, "Out of memory.\n");
  }
  for(i=0;i<PARALLEL_MODELS;i++){
    global_ob_idx[i] = 0;
  }
}

void clean_up_output_buffers(int output_fd, unsigned int gridsize){
  if (simex_buffer_indexing) {
    free_indexed_output_buffer(global_ixob, gridsize);
  }

  if(simex_output_files){
    free(global_ob);
  }
  else{
    munmap(global_ob, global_ob_count*sizeof(output_buffer));
    close(output_fd);
  }
  free(global_ob_idx);
}

/* Allocates memory for solver properties of each iterator. */
solver_props *alloc_solver_props(unsigned int gridsize, unsigned int blocksize) {
  int i;
  solver_props *props = (solver_props * )malloc(NUM_ITERATORS*sizeof(solver_props));

  // System data shared by all iterators
  top_systemstatedata *system_ptrs = (top_systemstatedata *)malloc(sizeof(top_systemstatedata));
  
  #if NUM_OUTPUTS > 0
  output_data *od = (output_data*)malloc(PARALLEL_MODELS*sizeof(output_data));
  unsigned int outputsize = sizeof(output_data)/sizeof(CDATAFORMAT);
  #else
  void *od = NULL;
  unsigned int outputsize = 0;
  #endif
 
  for (i=0; i<NUM_ITERATORS; i++) {
    props[i].initialized = 0;

    props[i].od = od;
    props[i].outputsize = outputsize;

    props[i].system_states = system_ptrs;

    props[i].time = (CDATAFORMAT *)malloc(gridsize*blocksize*sizeof(CDATAFORMAT));
    props[i].next_time = (CDATAFORMAT *)malloc(gridsize*blocksize*sizeof(CDATAFORMAT));
    props[i].count = NULL; // Allocated only by discrete solvers
    props[i].running = (int *)malloc(gridsize*blocksize*sizeof(int));
    props[i].last_iteration = (int*)malloc(gridsize*blocksize*sizeof(int));
  }

  return props;
}

/* Releases memory allocated for solver properties. */
void free_solver_props(solver_props *props) {
  int i;
  assert(props);

  #ifdef TARGET_GPU
  systemstatedata_external *system_states_next = NULL;
  #else
  systemstatedata_internal *system_states_int = NULL;
  systemstatedata_internal *system_states_next = NULL;
  #endif
  
  for(i=0;i<NUM_ITERATORS;i++){
    free(props[i].time);
    free(props[i].next_time);
    free(props[i].count);
    free(props[i].running);
    free(props[i].last_iteration);
    if(!system_states_next && (props[i].statesize + props[i].algebraic_statesize > 0)){
      system_states_next = (systemstatedata_external*)props[i].next_states;
      #if !defined TARGET_GPU
      system_states_int = (systemstatedata_internal*)props[i].model_states;
      #endif
    }
  }

  // System data shared by all iterators
  free(system_states_next);
  #if !defined TARGET_GPU
  free(system_states_int);
  #endif
  free(props->od);
  free(props->system_states);

  free(props);
}

void initial_system_data (int resuming, fn_mdlvar__t_input *inputs, fn_mdlvar__t_state *states, fn_mdlvar__t_output *outputs, unsigned int num_models, unsigned int modelid_offset, unsigned int gridsize, unsigned int blocksize) {
  unsigned int modelid = modelid_offset;
  unsigned int blockid, threadid;
  for (blockid=0; blockid<gridsize; blockid++) {
    for (threadid=0; threadid<blocksize; threadid++, modelid++) {
      initial_model_inputs(&inputs[blockid], modelid, threadid);
      if (!resuming) {
	initial_system_states(&inputs[blockid], &states[blockid], modelid, threadid);
      }
      initial_system_outputs(&inputs[blockid], &states[blockid], &outputs[blockid], modelid, threadid);
      initial_system_inputs(&inputs[blockid], &states[blockid], modelid, threadid);
    }
  }
}

/* FIXME this should obviously be generated code. */
/* Prepares model-specific solver properties for execution. */
void init_solver_props(solver_props *props, CDATAFORMAT starttime, CDATAFORMAT stoptime, unsigned int num_models, CDATAFORMAT *model_states, unsigned int modelid_offset, unsigned int gridsize, unsigned int blocksize) {
  // System data shared by all iterators
  top_systemstatedata *system_ptrs = props->system_states;

  #if defined TARGET_GPU
  systemstatedata_external *system_states_int = (systemstatedata_external*)model_states;
  systemstatedata_external *system_states_next = NULL;
  if (props->initialized) {
    // Reuse previously-allocated memory if possible.
    for(i=0;i<NUM_ITERATORS;i++){
      if(props[i].statesize + props[i].algebraic_statesize > 0){
	system_states_next = (systemstatedata_external*)props[i].next_states;
	break;
      }
    }
  }
  else {
    system_states_next = (systemstatedata_external*)malloc(sizeof(systemstatedata_external));
  }
  #else
  systemstatedata_internal *system_states_int = NULL;
  systemstatedata_internal *system_states_next = NULL;
  if (props->initialized) {
    for(i=0;i<NUM_ITERATORS;i++){
      if(props[i].statesize + props[i].algebraic_statesize > 0){
	system_states_int = (systemstatedata_internal*)props[i].model_states;
	system_states_next = (systemstatedata_internal*)props[i].next_states;
	break;
      }
    }
  }
  else {
    system_states_int = (systemstatedata_internal*)malloc(sizeof(systemstatedata_internal));
    system_states_next = (systemstatedata_internal*)malloc(sizeof(systemstatedata_internal));
  }
  #endif

  system_ptrs->mdlvar__t = props[ITERATOR_t].time;
  system_ptrs->states_mdlvar__t = system_states_int->states_mdlvar__t;
  system_ptrs->states_mdlvar__t_next = system_states_next->states_mdlvar__t;

  props[ITERATOR_t].initialized = 1;
  props[ITERATOR_t].solver = DORMAND_PRINCE;
  props[ITERATOR_t].iterator = ITERATOR_t;
  props[ITERATOR_t].timestep = global_timestep ? global_timestep : 0.1;
  props[ITERATOR_t].abstol = 1E-6;
  props[ITERATOR_t].reltol = 1E-5;
  props[ITERATOR_t].starttime = starttime;
  props[ITERATOR_t].stoptime = stoptime;
  props[ITERATOR_t].gridsize = gridsize;
  props[ITERATOR_t].blocksize = blocksize;
  props[ITERATOR_t].num_models = num_models;
  props[ITERATOR_t].modelid_offset = modelid_offset;
  props[ITERATOR_t].inputsize = NUM_INPUTS;
  props[ITERATOR_t].statesize = 2;
  props[ITERATOR_t].algebraic_statesize = 0;

  // Initial values moved to model_states first time through the exec
  props[ITERATOR_t].model_states = (CDATAFORMAT*)(&system_states_int->states_mdlvar__t);
  props[ITERATOR_t].next_states = (CDATAFORMAT*)(&system_states_next->states_mdlvar__t);

  memset(props[ITERATOR_t].last_iteration, 0, gridsize*blocksize*sizeof(int));
  memset(props[ITERATOR_t].running, 1, num_models*sizeof(int));
  if (num_models < gridsize * blocksize) {
    memset(props[ITERATOR_t].running+num_models, 0, (gridsize*blocksize-num_models)*sizeof(int));
  }

  for (modelid=0; modelid<num_models; modelid++) {
    props[ITERATOR_t].time[modelid] = starttime;
    props[ITERATOR_t].next_time[modelid] = starttime;
  }

  #if defined TARGET_GPU
  memcpy(system_states_next, system_states_int, sizeof(systemstatedata_external));
  #else
  memcpy(system_states_next, system_states_int, sizeof(systemstatedata_internal));
  #endif
}


// simengine_runmodel()
//
//    executes the model for the given parameters, states and simulation time
simengine_result *simengine_runmodel(simengine_opts *opts){
  double start_time = opts->start_time;
  double stop_time = opts->stop_time;
  unsigned int num_models = opts->num_models;
  const char *outputs_dirname = opts->outputs_dirname;

  CDATAFORMAT *model_states = (CDATAFORMAT *)malloc(PARALLEL_MODELS * NUM_STATES * sizeof(CDATAFORMAT));
  unsigned int stateid;
  unsigned int modelid;

  unsigned int models_executed;
  unsigned int models_per_batch;

  double *progress;
  int progress_fd;

  int output_fd;

  int resuming = 0;
  int random_initialized = 0;

  unsigned int blocksize = BLOCK_WIDTH(PARALLEL_MODELS);
  unsigned int gridsize = GRID_WIDTH(PARALLEL_MODELS);

  solver_props *props = alloc_solver_props(gridsize, blocksize);

# if defined TARGET_GPU
  gpu_init();
# endif

  open_progress_file(outputs_dirname, &progress, &progress_fd, num_models);

  // FIXME this must be generated or generalized
  // Create hierarchical model data structures
  fn_mdlvar__t_input *inputs = (fn_mdlvar__t_input *)malloc(gridsize*sizeof(fn_mdlvar__t_input));
  fn_mdlvar__t_state *states = (fn_mdlvar__t_input *)malloc(gridsize*sizeof(fn_mdlvar__t_state));
  fn_mdlvar__t_output *outputs = (fn_mdlvar__t_output *)malloc(gridsize*sizeof(fn_mdlvar__t_output));
	     
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

  init_output_buffers(outputs_dirname, &output_fd, gridsize, blocksize);

  // Run the parallel simulation repeatedly until all requested models have been executed
  for(models_executed = 0 ; models_executed < num_models; models_executed += PARALLEL_MODELS){
    unsigned int modelid_offset = global_modelid_offset + models_executed;
    models_per_batch = MIN(num_models - models_executed, PARALLEL_MODELS);
    /* Simulates one grid of parallel models.
     *
     * Before begining the loop, initialize the input, state, and
     * output vectors for the zeroth generation.
     * Evaluate user inputs first. User inputs may be read from
     * external sources or default to constant values.
     * Next evaluate state initial values. State initial value
     * equations may include constants or user inputs.
     * Then evaluate the entire system of outputs.
     * Finally evaluate the entire system of outputs.
     * 
     * The loop then proceeds recursively with the computed results of each
     * generation becoming the working data of the next generation,
     * terminating when the final generation has been reached.
     *
     * After the loop terminates, obtain a copy of the state and
     * output vectors of the final generation.
     */

    // Read initial data from optional user-specified files
    read_user_inputs(outputs_dirname, num_models, models_per_batch, modelid_offset);
    resuming = read_user_states(outputs_dirname, num_models, models_per_batch, modelid_offset);


    // Initialize the solver properties and internal simulation memory structures
    init_solver_props(props, start_time, stop_time, models_per_batch, model_states, models_executed+global_modelid_offset, gridsize, blocksize);

    // Populate the initial values of the hierarchical model data structures
    initial_system_data(resuming, inputs, states, outputs, models_per_batch, modelid_offset, gridsize, blocksize);

    // Run the model
    seresult->status = exec_loop(props, outputs_dirname, progress + models_executed, resuming);
    seresult->status_message = (char*) simengine_errors[seresult->status];

    return_model_outputs();
    return_system_states();

    if (SUCCESS != seresult->status) {
      break;
    }


    continue;




    
    // Copy inputs and state initial values to internal representation
#if NUM_CONSTANT_INPUTS > 0
#if defined TARGET_GPU
    host_constant_inputs = (CDATAFORMAT *)malloc(PARALLEL_MODELS * NUM_CONSTANT_INPUTS * sizeof(CDATAFORMAT));
#else
    host_constant_inputs = constant_inputs;
#endif
#else
    CDATAFORMAT *host_constant_inputs = NULL;
#endif

#if NUM_SAMPLED_INPUTS > 0
#if defined TARGET_GPU
    host_sampled_inputs = (sampled_input_t *)malloc(STRUCT_SIZE * NUM_SAMPLED_INPUTS * sizeof(sampled_input_t));
#else
    host_sampled_inputs = sampled_inputs;
#endif
#else
    sampled_input_t *host_sampled_inputs = NULL;
#endif

    resuming = initialize_states(model_states, outputs_dirname, num_models, models_per_batch, modelid_offset);
    initialize_inputs(host_constant_inputs, host_sampled_inputs, outputs_dirname, num_models, models_per_batch, modelid_offset, start_time);

#if defined TARGET_GPU && NUM_CONSTANT_INPUTS > 0
    CDATAFORMAT *g_constant_inputs;
    cutilSafeCall(cudaGetSymbolAddress((void **)&g_constant_inputs, constant_inputs));
    cutilSafeCall(cudaMemcpy(g_constant_inputs, host_constant_inputs, PARALLEL_MODELS * NUM_CONSTANT_INPUTS * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
#endif

#if defined TARGET_GPU && NUM_SAMPLED_INPUTS > 0
    sampled_input_t *g_sampled_inputs;
    cutilSafeCall(cudaGetSymbolAddress((void **)&g_sampled_inputs, sampled_inputs));
    cutilSafeCall(cudaMemcpy(g_sampled_inputs, host_sampled_inputs, STRUCT_SIZE * NUM_SAMPLED_INPUTS * sizeof(sampled_input_t), cudaMemcpyHostToDevice));
#endif

    // Initialize the solver properties and internal simulation memory structures
    solver_props *props = init_solver_props(start_time, stop_time, models_per_batch, model_states, models_executed+global_modelid_offset, gridsize, blocksize);

    // Initialize random number generator
    if (!random_initialized || opts->seeded) {
      random_init(models_per_batch);
      random_initialized = 1;
    }

    // If no initial states were passed in
    if(!resuming){
      if(seint.num_states > 0){
	// Initialize default states in next_states
	for(modelid=0;modelid<models_per_batch;modelid++){
	  init_states(props, modelid);
	  // Copy states from next_states to model_states
	  unsigned int iterid;
	  for(iterid=0;iterid<seint.num_iterators;iterid++){
	    solver_writeback(&props[iterid], modelid);
	  }
	}
      }
    }

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

  free(model_states);

  close_progress_file(progress, progress_fd, num_models);
  clean_up_output_buffers(output_fd, gridsize);

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

void check_inputs(const char *inputs_arg){
  int numi = 1;
  int leni = strlen(inputs_arg);
  unsigned int i,j;
  char *inputs;
  char *inp;

  if(leni == 0){
    USER_ERROR(Simatra:Simex:check_inputs, "No value passed to --inputs.");
  }

  inputs = (char*)malloc(leni+1);
  strcpy(inputs,inputs_arg);

  // Count inputs
  for(i=0;i<leni;i++){
    if(inputs[i] == ':'){
      numi++;
      inputs[i] = 0;
    }
  }

  // Make sure that the inputs all match valid input names
  inp = inputs;
  for(i=0;i<numi;i++){
    for(j=0;j<seint.num_inputs;j++){
      if(0 == strcmp(inp, seint.input_names[j])){
	break;
      }
    }
    if(j == seint.num_inputs){
      USER_ERROR(Simatra:Simex:check_inputs,"Model %s has no input with name '%s'.", seint.name, inp);
    }
    inp += strlen(inp) + 1;
  }

  free(inputs);
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
	USER_ERROR(Simatra:Simex:parse_args, "Start time can only be specified once.");
      }
      opts->start_time = atof(optarg);
      if(!__finite(opts->start_time)){
	USER_ERROR(Simatra:Simex:parse_args, "Start time is invalid %f.", opts->start_time);
      }
      break;
    case STOP:
      if(opts->stop_time){
	USER_ERROR(Simatra:Simex:parse_args, "Stop time can only be specified once.");
      }
      opts->stop_time = atof(optarg);
      if(!__finite(opts->stop_time)){
	USER_ERROR(Simatra:Simex:parse_args, "Stop time is invalid %f.", opts->stop_time);
      }
      break;
    case SEED:
      if(opts->seeded){
	USER_ERROR(Simatra:Simex:parse_args, "Random seed can only be specified once.");
      }
      opts->seeded = 1;
      opts->seed = atoi(optarg);
      break;
#ifdef TARGET_GPU
    case GPUID:
      if(opts->gpuid){
	USER_ERROR(Simatra:Simex:parse_args, "GPU ID can only be specified once.");
      }
      opts->gpuid = 1;
      global_gpuid = atoi(optarg);
      break;
#endif
    case INSTANCES:
      if(opts->num_models){
	USER_ERROR(Simatra:Simex:parse_args, "Number of model instances can only be specified once.");
      }
      opts->num_models = (unsigned int)strtod(optarg, NULL); // Handles 1E3 etc.
      if(opts->num_models < 1){
	USER_ERROR(Simatra:Simex:parse_args, "Invalid number of model instances %d", opts->num_models);
      }
      break;
    case INSTANCE_OFFSET:
      if(global_modelid_offset){
	USER_ERROR(Simatra:Simex:parse_args, "Model instance offset can only be specified once.");
      }
      global_modelid_offset = atoi(optarg);
      break;
    case INPUTS:
      check_inputs(optarg);
      break;
    case OUTPUT_DIR:
      if(opts->outputs_dirname){
	USER_ERROR(Simatra:Simex:parse_args, "Only one output file can be specified. '%s' OR '%s'", 
	      opts->outputs_dirname, optarg);
      }
      opts->outputs_dirname = optarg;
      break;
    case BINARY:
      if(binary_files){
	USER_ERROR(Simatra:Simex:parse_args, "Option '--binary' can only be specified once.");
      }
      binary_files = 1;
      break;
    case INTERFACE:
      print_interface();
      exit(0);
      break;
    case JSON_INTERFACE:
      {
	// The existence of this file is used as a semaphore between the simulation
	// and the MATLAB client. Writes a temporary file and renames it
	// to prevent the MATLAB client from attempting to read a partial file.
	FILE *json_file;
	char tmp[PATH_MAX];
	char *dupdir = strdup(optarg);

	strncpy(tmp, dirname(dupdir), PATH_MAX-1);
	if (strlen(tmp) > 0)
	  strncat(tmp, "/json-interface.tmp", PATH_MAX-1 - strlen(tmp));
	else
	  strncpy(tmp, "json-interface.tmp", PATH_MAX-1 - strlen(tmp));

	free(dupdir);
	
	json_file = fopen(tmp, "w");
	if(!json_file){
	  ERROR(Simatra:Simex:parse_args, "Could not open file '%s' to write json interface.", tmp);
	}
	fprintf(json_file, json_interface, sizeof(CDATAFORMAT), sizeof(void*), PARALLEL_MODELS, BUFFER_LEN);
	fclose(json_file);

	if (0 != rename(tmp, optarg)) {
	  ERROR(Simatra:Simex:parse_args, "Could not rename json interface '%s' to file '%s': %s.", tmp, optarg, strerror(errno));
	}
      }
      break;
    case SHARED_MEMORY:
      simex_output_files = 0;
      break;
    case BUFFER_COUNT:
      if(global_ob_count != 2){
	USER_ERROR(Simatra:Simex:parse_args, "Number of model instances can only be specified once.");
      }
      global_ob_count = (unsigned int)strtod(optarg, NULL); // Handles 1E3 etc.
      if(global_ob_count < 1){
	USER_ERROR(Simatra:Simex:parse_args, "Invalid number of output buffers %d", global_ob_count);
      }
      break;
    case BUFFER_INDEXING:
      simex_buffer_indexing = 1;
    case MAX_ITERS:
      MAX_ITERATIONS = (unsigned int)strtod(optarg, NULL); // Handles 1E3 etc.
      if(MAX_ITERATIONS < 1){
	USER_ERROR(Simatra:Simex:parse_args, "Invalid number of max iterations %d", MAX_ITERATIONS);
      }      
      break;
    case GPU_BLOCK_SZ:
      GPU_BLOCK_SIZE = (unsigned int)strtod(optarg, NULL); // Handles 1E3 etc.
      if(GPU_BLOCK_SIZE < 1){
	USER_ERROR(Simatra:Simex:parse_args, "Invalid gpu block size %d", GPU_BLOCK_SIZE);
      }            
      break;
      // HACK BEGIN
    case ALL_TIMESTEPS:
      global_timestep = strtod(optarg, NULL);
      break;
      // HACK END
    default:
      // Stop execution if an invalid command line option is found.
      // Force the user to correct the error instead of ignoring options that
      // are not understood. Otherwise a typo could lead to executing a simulation
      // with undesired default options.
      USER_ERROR(Simatra:Simex:parse_args, "Invalid argument");
    }
  }

  // Check that no invalid parameters were passed to simulation
  if(optind < argc){
    PRINTFE("\n");
    while(optind < argc)
      PRINTFE("\t'%s'\n", argv[optind++]);
    USER_ERROR(Simatra:Simex:parse_args, "Invalid parameters passed to simex:");
  }

  // Ensure that the stop time is later than the start time.  If they are equal,
  // (i.e. not set, default to 0) the model interface will be returned.
  if(opts->stop_time < opts->start_time){
    USER_ERROR(Simatra:Simex:parse_args, "stop time (%f) must be greater than start time (%f)",
	  opts->stop_time, opts->start_time);
  }

  // Quit if we don't have anything to do
  // this will be the case when the user only wants the interface
  if(opts->stop_time == opts->start_time){
    exit(0);
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
      ERROR(Simatra:Simex:parse_args, "Could not create output directory %s.",
	    opts->outputs_dirname);
    }
  }

  if(opts->num_models > MAX_NUM_MODELS){
    USER_ERROR(Simatra:Simex:parse_args, "Number of model instances must be less than %d, requested %d.", MAX_NUM_MODELS, opts->num_models);
  }
  if(global_modelid_offset > MAX_NUM_MODELS){
    USER_ERROR(Simatra:Simex:parse_args, "Model instance offset must be less than %d, requested %d.", MAX_NUM_MODELS, global_modelid_offset);
  }

  long long sanity_check = 0;
  sanity_check += global_modelid_offset;
  sanity_check += opts->num_models;

  if(sanity_check > MAX_NUM_MODELS){
    USER_ERROR(Simatra:Simex:parse_args, "Number of model instances (%d) too large for requested model instance offset (%d)."
	  "Maximum number of models is %d.", opts->num_models, global_modelid_offset, MAX_NUM_MODELS);
  }
  

  // Successful parsing of command line arguments
  return 0;
}

#define BYTE(val,n) ((val>>(n<<3))&0xff) // Also used in log_outputs

// This will create model directories for inputs/outputs if they weren't created before calling this simulation
void make_model_directories(simengine_opts *opts){
#if NUM_OUTPUTS > 0
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
	  ERROR(Simatra::Simex::make_model_directories, "Could not create intermediate directory '%s'", model_dirname);
	}
      }
    }
    // Create the outputs directory
    sprintf((model_dirname + strlen(model_dirname)), "/outputs");
    if(mkdir(model_dirname, 0777)){
	  ERROR(Simatra::Simex::make_model_directories, "Output directory '%s' already exists, remove manually or specify a new output directory with the --outputdir <directory name> option", opts->outputs_dirname);
    }
  }
#endif
}

void write_states_time(simengine_opts *opts, simengine_result *result){
  // Make sure a directory for the model exists
  char states_time_filename[PATH_MAX];
  FILE *states_time_file;
  unsigned int modelid, stateid;
  long position;

  // Write final states
  position = global_modelid_offset * seint.num_states * sizeof(double);
  sprintf(states_time_filename, "%s/final-states", opts->outputs_dirname);
  states_time_file = fopen(states_time_filename, "w");
  if(NULL == states_time_file){
    ERROR(Simatra::Simex::write_states_time, "could not open file '%s'", states_time_filename);
  }
  if(-1 == fseek(states_time_file, position, SEEK_SET)){
    ERROR(Simatra::Simex::write_states_time, "could not seek to position %ld in file '%s'", position, states_time_filename);
  }
  if(opts->num_models * seint.num_states != fwrite(result->final_states, sizeof(double), opts->num_models * seint.num_states, states_time_file)){
    ERROR(Simatra::Simex::write_states_time, "could not write to file '%s'", states_time_filename);
  }
  fclose(states_time_file);

  // Write final time
  position = global_modelid_offset * sizeof(double);
  sprintf(states_time_filename, "%s/final-time", opts->outputs_dirname);
  states_time_file = fopen(states_time_filename, "w");
  if(NULL == states_time_file){
    ERROR(Simatra::Simex::write_states_time, "could not open file '%s'", states_time_filename);
  }
  if(-1 == fseek(states_time_file, position, SEEK_SET)){
    ERROR(Simatra::Simex::write_states_time, "could not seek to position %ld in file '%s'", position, states_time_filename);
  }
  if(opts->num_models != fwrite(result->final_time, sizeof(double), opts->num_models, states_time_file)){
    ERROR(Simatra::Simex::write_states_time, "could not write to file '%s'", states_time_filename);
  }
  fclose(states_time_file);
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
    USER_ERROR(Simatra:Simex:main, "--binary not specified and ascii data is not currently supported.");

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

    if(simex_output_files){
      make_model_directories(&opts);
    }

    simengine_result *result = simengine_runmodel(&opts);

    if (SUCCESS == result->status){
      write_states_time(&opts, result);
    }
    else{
      WARN(Simatra:Simex:runmodel, "Simulation returned error %d: %s",
	      result->status, result->status_message);
    }

    return 0;
  }
}
