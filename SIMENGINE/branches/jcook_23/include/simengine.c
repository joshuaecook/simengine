// This is included as a separate file to keep a division of the API interface from
// internal code
#include<simengine_api.c>

#define MAX_ALLOC_SIZE 65536000
#define MAX_ITERATIONS 1000
#define GPU_BLOCK_SIZE 192


/*************************************************************************************
 *
 * Output data handling routines
 *
 ************************************************************************************/

__DEVICE__ void init_output_buffer(output_buffer *ob, unsigned int modelid){
  ob->full[modelid] = 0;
  ob->count[modelid] = 0;
  ob->ptr[modelid] = &ob->buffer[modelid*BUFFER_LEN];
  ob->end[modelid] = &ob->buffer[(modelid+1)*BUFFER_LEN];
}

/* Transmutes the internal data buffer into the structured output
 * which may be retured to the client.
 */
int log_outputs(output_buffer *ob, simengine_output *outputs, unsigned int modelid) {
  unsigned int outputid, nquantities, dataid, quantityid;
  simengine_output *output;
  double *odata;
	     
  unsigned int ndata = ob->count[modelid];
  ob_data *obd = (ob_data*)(&(ob->buffer[modelid * BUFFER_LEN]));
  CDATAFORMAT *data;
	     
  fprintf(stderr, "logging %d data for model %d\n", ndata, modelid);

  for (dataid = 0; dataid < ndata; ++dataid) {
    outputid = obd->outputid;
    nquantities = obd->nquantities;
    data = obd->data;
		 
    // TODO an error code for invalid data?
    if (outputid > seint.num_outputs) { return 1; }
    if (seint.output_num_quantities[outputid] != nquantities) { return 1; }
		 
    output = &outputs[AS_IDX(seint.num_outputs,semeta.num_models,outputid,modelid)];
    if (output->num_samples >= output->alloc)
	{
	output->alloc *= 2;
#pragma omp critical
	    {
	    PRINTF("reallocating for %d data for model %d output %d\n", output->alloc, modelid, outputid);
	    output->data = (double*)se_alloc.realloc(output->data, output->num_quantities * output->alloc * sizeof(double));
	    }
	if (!output->data)
	    { return ERRMEM; }
	}

		 
    odata = &output->data[AS_IDX(nquantities, output->num_samples, 0, output->num_samples)];

    memcpy(odata, obd->data, nquantities * sizeof(CDATAFORMAT));

    obd = (ob_data*)(&(obd->data[nquantities]));
    ++output->num_samples;
  }
	     
  return 0;
}

/*************************************************************************************
 *
 * CPU serial and parallel execution routines
 *
 ************************************************************************************/
#ifndef TARGET_GPU
// Run a single model to completion on a single processor core
int exec_cpu(INTEGRATION_MEM *mem, simengine_output *outputs, unsigned int modelid){
  // Run simulation to completion
//  while(mem->props->time[modelid] < mem->props->stoptime){
  while(mem->props->running[modelid]){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(mem->props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full
    while(0 == ((output_buffer *)(mem->props->ob))->full[modelid]){
      // Check if simulation is complete (or produce only a single output if there are no states)
      if(!mem->props->running[modelid] || mem->props->statesize == 0){
	mem->props->running[modelid] = 0;
	((output_buffer*)(mem->props->ob))->finished[modelid] = 1;
#if NUM_OUTPUTS > 0
	// Log output values for final timestep
	// Run the model flows to ensure that all intermediates are computed, mem->k1 is borrowed from the solver as scratch for ignored dydt values
	model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);
	// Buffer the last values
	buffer_outputs(mem->props->time[modelid],((output_data*)mem->props->outputs), ((output_buffer*)mem->props->ob), modelid);
#endif
	break;
      }
		 
      CDATAFORMAT prev_time = mem->props->time[modelid];

      // Execute solver for one timestep
      SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);

#if NUM_OUTPUTS > 0
      // Store a set of outputs only if the sovler made a step
      if (mem->props->time[modelid] > prev_time) {
	buffer_outputs(prev_time, (output_data*)mem->props->outputs, ((output_buffer*)mem->props->ob), modelid);
      }
#endif
    }
    // Log outputs from buffer to external api interface
    if(0 != log_outputs((output_buffer*)mem->props->ob, outputs, modelid)){
      return ERRMEM;
    }
  }
  
  return SUCCESS;
}

// Run all models in parallel batches on all available processor cores
int exec_parallel_cpu(INTEGRATION_MEM *mem, simengine_output *outputs){
  int ret = SUCCESS;
  // Initialize omp with one thread per processor core
  omp_set_num_threads(omp_get_num_procs());

  // Start threads
#pragma omp parallel
  {
    int status;
    unsigned int modelid;
    unsigned int thread_num = omp_get_thread_num();
    unsigned int num_threads = omp_get_num_threads();
		 
    unsigned int models_per_thread = NUM_MODELS/num_threads;
    unsigned int extra_models = NUM_MODELS%num_threads;
		 
    // Run models in batches equal to the number of cores
    for(modelid = thread_num*models_per_thread; modelid < (thread_num+1)*models_per_thread; modelid++){
      status = exec_cpu(mem, outputs, modelid);
      if(status != SUCCESS){
	ret = status;
	break;
      }
    }
    // If the number of models is not an even multiple of the number of cores
    // there will be one additional batch of models, with fewer threads than
    // the number of cores
    if(thread_num < extra_models){
      modelid = NUM_MODELS - extra_models + thread_num;
      status = exec_cpu(mem, outputs, modelid);
      if(status != SUCCESS){
	ret = status;
      }
    }
  }// Threads implicitly joined here
  return ret;
}

// Run all models serially on a single cpu core
int exec_serial_cpu(INTEGRATION_MEM *mem, simengine_output *outputs){
  int ret = SUCCESS;
  unsigned int modelid;

  for(modelid=0;modelid<NUM_MODELS;modelid++){
    ret = exec_cpu(mem, outputs, modelid);
    if(ret != SUCCESS){
      return ret;
    }
  }
  return ret;
}
#endif // ndef TARGET_GPU

/*************************************************************************************
 *
 * GPU execution routines
 *
 ************************************************************************************/

#if defined(TARGET_GPU)
// GPU execution kernel that runs each model instance for a number of iterations or until the buffer fills
__GLOBAL__ void exec_kernel_gpu(INTEGRATION_MEM *mem, unsigned int ob_id){
  const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;
  
  unsigned int num_iterations;
	     
  if (modelid < NUM_MODELS) {
    output_buffer *ob = (output_buffer *)mem->props->ob;
    if (ob_id) { ob = &(ob[ob_id]); }

    // Initialize output buffer to store output data
    init_output_buffer(ob, modelid);
    
    // Run up to MAX_ITERATIONS for each model
    for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){
      // Stop if simulation finished previously or if the output buffer is full.
      // Threads are launched in batches on the GPU and not all will complete at the
      // same time with variable timestep solvers.
      if(ob->finished[modelid] || ob->full[modelid]){
	break;
      }
      // Check if the simulation just finished (or if there are no states)
      if(!mem->props->running[modelid] || mem->props->statesize == 0) {
	mem->props->running[modelid] = 0;
	ob->finished[modelid] = 1;
#if NUM_OUTPUTS > 0
	// Log output values for final timestep
	// Run the model flows to ensure that all intermediates are computed, mem->k1 is borrowed from the solver as scratch for ignored dydt values
	model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);
	// Buffer the last values
	buffer_outputs(mem->props->time[modelid],(output_data*)mem->props->outputs, ob, modelid);
#endif
	break;
      }
      
      CDATAFORMAT prev_time = mem->props->time[modelid];

      // Execute solver for one timestep
      SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);

#if NUM_OUTPUTS > 0
      // Store a set of outputs only if the sovler made a step
      if (mem->props->time[modelid] > prev_time) {
	buffer_outputs(prev_time, (output_data*)mem->props->outputs, ob, modelid);
      }
#endif
    }
  }
}



int exec_parallel_gpu_mapped(INTEGRATION_MEM *mem, solver_props *props, simengine_output *outputs){
  int ret = SUCCESS;
  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  num_gpu_threads = GPU_BLOCK_SIZE < NUM_MODELS ? GPU_BLOCK_SIZE : NUM_MODELS;
  num_gpu_blocks = (NUM_MODELS + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;

  unsigned int active_models = NUM_MODELS;

  // Using 2 copies of the output buffer, capable devices may execute
  // a kernel while concurrently logging a previous result.
  output_buffer *ob = (output_buffer *)props->ob;
  unsigned int ob_id = 0;
  unsigned int aflight = 0; // Number of kernels in flight
  cudaEvent_t checkpoint[2]; // No more than 2 events in queue
  // These events signal a kernel completion. After logging, we sync
  // on the next event to await more results.
  cutilSafeCall(cudaEventCreate(&checkpoint[0]));
  cutilSafeCall(cudaEventCreate(&checkpoint[1]));

  // Initialize omp with one thread per processor core
  omp_set_num_threads(omp_get_num_procs());

  fprintf(stderr, "running %d models on %d cores.\n", NUM_MODELS, omp_get_num_procs());

  // Log outputs using parallel host threads
#pragma omp parallel
  {
    unsigned int modelid;
    unsigned int thread_num = omp_get_thread_num();
    unsigned int num_threads = omp_get_num_threads();
    
    unsigned int models_per_thread = NUM_MODELS / num_threads;
    unsigned int extra_models = NUM_MODELS % num_threads;

    // Only Host thread 0 may interact with the GPU
    unsigned int thread0 = 0 == thread_num;
		 
    while(SUCCESS == ret && active_models){

      if (aflight) {
	// A kernel is executing; wait for it to complete
	if(thread0){
	  cutilSafeCall(cudaEventSynchronize(checkpoint[ob_id]));
	  --aflight;
	}

#pragma omp barrier

	// Copies important data between output buffers
	for(modelid = thread_num*models_per_thread; modelid < (thread_num+1)*models_per_thread; modelid++) {
	  ob[1^ob_id].finished[modelid] = ob[ob_id].finished[modelid];
	  if (ob[ob_id].finished[modelid]) {
#pragma omp critical 
	    { --active_models; }
	  }
	}
	if (thread_num < extra_models) {
	  modelid = NUM_MODELS - extra_models + thread_num;
	  ob[1^ob_id].finished[modelid] = ob[ob_id].finished[modelid];
	  if (ob[ob_id].finished[modelid]) {
#pragma omp critical
	    { --active_models; }
	  }
	}

#pragma omp barrier

	if (thread0) {
	  ob_id ^= 1;
	  // Launches the next kernel
	  if (active_models) {
	      PRINTF("launching next kernel with %d active_models (ob %d)\n", active_models, ob_id);
	    exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(mem, ob_id);
	    cutilSafeCall(cudaEventRecord(checkpoint[ob_id], 0));
	    ++aflight;
	  }
	}

#pragma omp barrier

	  // Copies data in parallel to external API data structures
	  for(modelid = thread_num*models_per_thread; modelid < (thread_num+1)*models_per_thread; modelid++) {
	    if (SUCCESS != log_outputs(&(ob[1^ob_id]), outputs, modelid)) {
	      ret = ERRMEM;
	      break;
	    }
	  }
	  // If the number of models is not an even multiple of the number of cores
	  // there will be one additional batch of models, with fewer threads than
	  // the number of cores
	  if (thread_num < extra_models) {
	    modelid = NUM_MODELS - extra_models + thread_num;
	    if (SUCCESS != log_outputs(&(ob[1^ob_id]), outputs, modelid)) {
	      ret = ERRMEM;
	      break;
	    }
	  }
      }
      else if (active_models) {
	// No kernels active; launch one
	if (thread0) {
	    PRINTF("launching first kernel with %d active_models (ob %d)\n", active_models, ob_id);
	  exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(mem, ob_id);
	  cutilSafeCall(cudaEventRecord(checkpoint[ob_id], 0));
	  ++aflight;
	}
      }




    }
    // Host threads implicitly join here
  }
  // Copy final times and states from GPU
  cutilSafeCall(cudaMemcpy(props->time, props->gpu.time, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
  cutilSafeCall(cudaMemcpy(props->model_states, props->gpu.model_states, props->statesize*props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));

  return ret;
}


int exec_parallel_gpu(INTEGRATION_MEM *mem, solver_props *props, simengine_output *outputs){
  int ret = SUCCESS;
  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  num_gpu_threads = GPU_BLOCK_SIZE < NUM_MODELS ? GPU_BLOCK_SIZE : NUM_MODELS;
  num_gpu_blocks = (NUM_MODELS + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;

  unsigned int active_models = NUM_MODELS;
  output_buffer *ob = (output_buffer *)props->ob;

  // Initialize omp with one thread per processor core
  omp_set_num_threads(omp_get_num_procs());

  fprintf(stderr, "running %d models on %d cores.\n", NUM_MODELS, omp_get_num_procs());

  // Log outputs using parallel host threads
#pragma omp parallel
  {
    unsigned int modelid;
    unsigned int thread_num = omp_get_thread_num();
    unsigned int num_threads = omp_get_num_threads();
    
    unsigned int models_per_thread = NUM_MODELS / num_threads;
    unsigned int extra_models = NUM_MODELS % num_threads;

    // Only Host thread 0 may interact with the GPU
    unsigned int thread0 = 0 == thread_num;
		 
    while(SUCCESS == ret && active_models)
	{
	if (thread0) 
	    {
	    PRINTF("launching next kernel with %d active_models\n", active_models);
	    exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(mem,0);
	    cutilSafeCall(cudaMemcpy(ob, props->gpu.ob, props->ob_size, cudaMemcpyDeviceToHost));
	    }

#pragma omp barrier

	  // Copies data in parallel to external API data structures
	  for(modelid = thread_num*models_per_thread; modelid < (thread_num+1)*models_per_thread; modelid++) {
	    if (SUCCESS != log_outputs(ob, outputs, modelid)) {
	      ret = ERRMEM;
	      break;
	    }
	    if (ob->finished[modelid]) 
		{
#pragma omp critical
		--active_models;
		}
	  }
	  // If the number of models is not an even multiple of the number of cores
	  // there will be one additional batch of models, with fewer threads than
	  // the number of cores
	  if (thread_num < extra_models) {
	    modelid = NUM_MODELS - extra_models + thread_num;
	    if (SUCCESS != log_outputs(ob, outputs, modelid)) {
	      ret = ERRMEM;
	      break;
	    }
	    if (ob->finished[modelid]) 
		{
#pragma omp critical
		--active_models;
		}
	  }

	}
    // Host threads implicitly join here
  }
  // Copy final times and states from GPU
  cutilSafeCall(cudaMemcpy(props->time, props->gpu.time, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
  cutilSafeCall(cudaMemcpy(props->model_states, props->gpu.model_states, props->statesize*props->num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));

  return ret;
}
#endif // defined(TARGET_GPU)


/*************************************************************************************
 *
 * Main execution routine
 *
 ************************************************************************************/
	 
int exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs) {
  unsigned int modelid = 0;
  unsigned int status = SUCCESS;
  output_buffer *ob;
#if NUM_OUTPUTS > 0
  output_data *od = (output_data*)malloc(NUM_MODELS*sizeof(output_data));
  unsigned int outputsize = sizeof(output_data)/sizeof(CDATAFORMAT);
#else
  void *od = NULL;
  unsigned int outputsize = 0;
#endif
  int *running = (int*)malloc(NUM_MODELS*sizeof(int));

  solver_props props;
  props.timestep = DT;
  props.abstol = ABSTOL;
  props.reltol = RELTOL;
  props.starttime = *t;
  props.stoptime = t1;
  props.time = t;
  props.model_states = model_states;
  props.inputs = inputs;
  props.outputs = (CDATAFORMAT*)od;
  props.statesize = NUM_STATES;
  props.inputsize = NUM_INPUTS;
  // This is the number of values needed to produce outputs including
  // values used in conditionals, not just the number of named outputs
  props.outputsize = outputsize;
  props.num_models = NUM_MODELS;
#if defined(TARGET_GPU)
  props.gpu.ob_mapped = 1;
  if (props.gpu.ob_mapped)
      { props.ob_size = 2 * sizeof(output_buffer); }
  else
      { props.ob_size = sizeof(output_buffer); }
#else
  props.ob_size = sizeof(output_buffer);
#endif
  ob = (output_buffer*)malloc(props.ob_size);
  props.ob = ob;
  props.running = running;
  
  // Initialize run status flags
  for(modelid=0;modelid<NUM_MODELS;modelid++){
    ob->finished[modelid] = 0;
    running[modelid] = 1;
  }

  INTEGRATION_MEM *mem = SOLVER(INTEGRATION_METHOD, init, TARGET, SIMENGINE_STORAGE, &props);


  // Execute the model(s) on the appropriate target
#if defined(TARGET_CPU)
  status = exec_serial_cpu(mem, outputs);
#elif defined(TARGET_OPENMP)
  status = exec_parallel_cpu(mem, outputs);
#elif defined(TARGET_GPU)
  PRINTF("executing on GPU\n");
  if (props.gpu.ob_mapped)
      { status = exec_parallel_gpu_mapped(mem, &props, outputs); }
  else
      { status = exec_parallel_gpu(mem, &props, outputs); }
#else
#error Invalid target
#endif
	     
  SOLVER(INTEGRATION_METHOD, free, TARGET, SIMENGINE_STORAGE, mem);
  free(ob);
  if(od) free(od);
  free(running);
  return status;
}
