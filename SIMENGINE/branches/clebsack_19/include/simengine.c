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
  void *data = &(ob->buffer[modelid * BUFFER_LEN]);
	     
  for (dataid = 0; dataid < ndata; ++dataid) {
    outputid = ((unsigned int *)data)[0];
    nquantities = ((unsigned int *)data)[1];
    data = &((unsigned int*)data)[2];
		 
    // TODO an error code for invalid data?
    if (outputid > seint.num_outputs) { return 1; }
    if (seint.output_num_quantities[outputid] != nquantities) { return 1; }
		 
    output = &outputs[AS_IDX(seint.num_outputs,semeta.num_models,outputid,modelid)];
		 
    if (output->num_samples == output->alloc) {
      output->alloc *= 2;
#pragma omp critical
      {
	output->data = (double*)se_alloc.realloc(output->data, output->num_quantities * output->alloc * sizeof(double));
      }
      if (!output->data)
	{ return 1; }
    }
		 
    odata = &output->data[AS_IDX(nquantities, output->num_samples, 0, output->num_samples)];
		 
    for (quantityid = 0; quantityid < nquantities; ++quantityid) {
      odata[quantityid] = *((CDATAFORMAT*)data);
      data = &((CDATAFORMAT*)data)[1];
    }
		 
    ++output->num_samples;
  }
	     
  return 0;
}

/*************************************************************************************
 *
 * CPU serial and parallel execution routines
 *
 ************************************************************************************/

// Run a single model to completion on a single processor core
int exec_cpu(INTEGRATION_MEM *mem, simengine_output *outputs, unsigned int modelid){
  // Run simulation to completion
//  while(mem->props->time[modelid] < mem->props->stoptime){
  while(mem->props->running[modelid]){
    // Initialize a temporary output buffer
    init_output_buffer((output_buffer*)(mem->props->ob), modelid);
 
    // Run a set of iterations until the output buffer is full
    while(0 == ((output_buffer *)(mem->props->ob))->full[modelid]){
      // Check if simulation is complete
//      if(mem->props->time[modelid] >= mem->props->stoptime){
      if(!mem->props->running[modelid]){
	((output_buffer*)(mem->props->ob))->finished[modelid] = 1;
	// The CPU kernel is used within the Parallel CPU kernel
#pragma omp critical
	--((output_buffer*)(mem->props->ob))->active_models;
	// Log output values for final timestep
	// Run the model flows to ensure that all intermediates are computed, mem->k1 is borrowed from the solver as scratch for ignored dydt values
	model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);
	// Buffer the last values
	buffer_outputs(mem->props->time[modelid],((output_data*)mem->props->outputs), ((output_buffer*)mem->props->ob), modelid);
	break;
      }
		 
      CDATAFORMAT prev_time = mem->props->time[modelid];

      // Execute solver for one timestep
      SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);

      // Store a set of outputs only if the sovler made a step
      if (mem->props->time[modelid] > prev_time) {
	buffer_outputs(prev_time, ((output_data*)mem->props->outputs), ((output_buffer*)mem->props->ob), modelid);
      }
      else {
	break;
      }
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
      modelid = num_threads * models_per_thread + thread_num;
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

/*************************************************************************************
 *
 * GPU execution routines
 *
 ************************************************************************************/

#if defined(TARGET_GPU)
// GPU execution kernel that runs each model instance for a number of iterations or until the buffer fills
__GLOBAL__ void exec_kernel_gpu(INTEGRATION_MEM *mem){
  const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;
  
  unsigned int num_iterations;
	     
  if (modelid < NUM_MODELS) {

    // Initialize output buffer to store output data
    init_output_buffer((output_buffer*)(mem->props->ob), modelid);
    
    // Run up to MAX_ITERATIONS for each model
    for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){
      // Check if simulation finished previously
      if(((output_buffer*)(mem->props->ob))->finished[modelid]){
	// (threads are launched in batches on the GPU and not all will complete at the
	// same time with variable timestep solvers)
	break;
      }
      // Check if the simulation just finished
//      if(mem->props->time[modelid] >= mem->props->stoptime){
      if(!mem->props->running[modelid]){
	((output_buffer*)(mem->props->ob))->finished[modelid] = 1;
	atomicDec(&((output_buffer*)(mem->props->ob))->active_models, NUM_MODELS);
	// Log output values for final timestep
	// Run the model flows to ensure that all intermediates are computed, mem->k1 is borrowed from the solver as scratch for ignored dydt values
	model_flows(mem->props->time[modelid], mem->props->model_states, mem->k1, mem->props->inputs, mem->props->outputs, 1, modelid);
	// Buffer the last values
	buffer_outputs(mem->props->time[modelid],((output_data*)mem->props->outputs), ((output_buffer*)mem->props->ob), modelid);
	break;
      }
      
      CDATAFORMAT prev_time = mem->props->time[modelid];

      // Execute solver for one timestep
      SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);

      // Store a set of outputs only if the sovler made a step
      if (mem->props->time[modelid] > prev_time) {
	buffer_outputs(prev_time, (output_data*)mem->props->outputs, (output_buffer*)mem->props->ob, modelid);
      }
      else {
	break;
      }
      
      // Stop if the output buffer is full
      if(((output_buffer*)(mem->props->ob))->full[modelid]) { 
	break; 
      }
    }
  }
}

int exec_parallel_gpu(INTEGRATION *mem, solver_props *props, simengine_output *outputs){
  int ret = SUCCESS;
  unsigned int num_gpu_threads;
  unsigned int num_gpu_blocks;
  num_gpu_threads = GPU_BLOCK_SIZE < NUM_MODELS ? GPU_BLOCK_SIZE : NUM_MODELS;
  num_gpu_blocks = (NUM_MODELS + GPU_BLOCK_SIZE - 1) / GPU_BLOCK_SIZE;
	     
  // Initialize omp with one thread per processor core
  omp_set_num_threads(omp_get_num_procs());

  // Log outputs using parallel host threads
#pragma omp parallel
  {
    int status;
    unsigned int modelid;
    unsigned int thread_num = omp_get_thread_num();
    unsigned int num_threads = omp_get_num_threads();
    
    unsigned int models_per_thread = NUM_MODELS/num_threads;
    unsigned int extra_models = NUM_MODELS%num_threads;
		 
    while(SUCCESS == ret && ob->active_models){
      // Only Host thread 0 can interact with the GPU
      if(0 == thread_num){
	// Execute models on the GPU
	exec_kernel_gpu<<<num_gpu_blocks, num_gpu_threads>>>(mem);
	// Copy data back to the host
	cutilSafeCall(cudaMemcpy(props.ob, props.gpu.ob, props.ob_size, cudaMemcpyDeviceToHost));
      }

      // Make sure all threads wait for GPU to produce data
#pragma omp barrier

      // Copy data in parallel to external api interface
      for(modelid = thread_num*models_per_thread; modelid < (thread_num+1)*models_per_thread; modelid++){
	status = log_outputs(ob, outputs, modelid);
	if(SUCCESS != status){
	  ret = ERRMEM;
	  break;
	}
      }
      // If the number of models is not an even multiple of the number of cores
      // there will be one additional batch of models, with fewer threads than
      // the number of cores
      if(thread_num < extra_models){   
	for (modelid = 0; modelid < semeta.num_models; ++modelid) {
	  status = log_outputs((output_buffer*)props->ob, outputs, modelid);
	  if (SUCCESS != status){
	    ret = ERRMEM;
	    break;
	  }
	}
      }
    } // Host threads implicitly join here
  }
  // Copy final times ans states from GPU
  cutilSafeCall(cudaMemcpy(props.time, props.gpu.time, props.num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
  cutilSafeCall(cudaMemcpy(props.model_states, props.gpu.model_states, props.statesize*props.num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));

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
  output_data *od = (output_data*)malloc(NUM_MODELS*sizeof(output_data));
  output_buffer *ob = (output_buffer*)malloc(sizeof(output_buffer));
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
  props.outputsize = sizeof(output_data)/sizeof(CDATAFORMAT);
  props.num_models = NUM_MODELS;
  props.ob_size = sizeof(output_buffer);
  props.ob = ob;
  props.running = running;
  
  // Initialize run status flags
  for(modelid=0;modelid<NUM_MODELS;modelid++){
    ob->finished[modelid] = 0;
    running[modelid] = 1;
  }
  ob->active_models = NUM_MODELS;
	     
  INTEGRATION_MEM *mem = SOLVER(INTEGRATION_METHOD, init, TARGET, SIMENGINE_STORAGE, &props);

  // Execute the model(s) on the appropriate target
#if defined(TARGET_CPU)
  status = exec_serial_cpu(mem, outputs);
#elif defined(TARGET_OPENMP)
  status = exec_parallel_cpu(mem, outputs);
#elif defined(TARGET_GPU)
  status = exec_parallel_gpu(mem, props, outputs);
#else
#error Invalid target
#endif
	     
  SOLVER(INTEGRATION_METHOD, free, TARGET, SIMENGINE_STORAGE, mem);
  free(ob);
  free(od);
  free(running);
  return status;
}
