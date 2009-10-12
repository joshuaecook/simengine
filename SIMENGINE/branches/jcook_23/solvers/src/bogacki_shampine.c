// Bogacki-Shampine (ode23) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"


#if defined TARGET_GPU
extern __shared__ CDATAFORMAT shmem[];
#endif

bogacki_shampine_mem *SOLVER(bogacki_shampine, init, TARGET, SIMENGINE_STORAGE, solver_props *props) {
  unsigned int i;
#if defined TARGET_GPU
  GPU_ENTRY(init, SIMENGINE_STORAGE);

  // Temporary CPU copies of GPU datastructures
  bogacki_shampine_mem tmem;
  // GPU datastructures
  bogacki_shampine_mem *dmem;

  CDATAFORMAT *temp_cur_timestep;

  // Computes GPU kernel geometry
  size_t shmem_per_thread, total_shmem = 1<<14;
  int warp_size = 1<<5;
  uint threads_per_block;
  uint num_gpu_threads;
  uint num_gpu_blocks;

  // shared space for model states and solver overhead
  shmem_per_thread = sizeof(CDATAFORMAT) * props->statesize * 8;
  // shared space for a vector of time
  shmem_per_thread += sizeof(CDATAFORMAT);
  // shared space for a vector of `running' flags
  shmem_per_thread += sizeof(int);

  
  threads_per_block = total_shmem / shmem_per_thread;
  threads_per_block = warp_size * (threads_per_block / warp_size);

  num_gpu_threads = threads_per_block < props->num_models ? threads_per_block : props->num_models;
  num_gpu_blocks = (props->num_models + threads_per_block - 1) / threads_per_block;

  props->gpu.blockx = num_gpu_threads;
  props->gpu.blocky = 1;
  props->gpu.blockz = 1;
  props->gpu.gridx = num_gpu_blocks;
  props->gpu.gridy = 1;
  props->gpu.gridz = 1;
  props->gpu.shmem_per_block = shmem_per_thread * num_gpu_threads;

  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(bogacki_shampine_mem)));
  tmem.props = GPU_ENTRY(init_props, SIMENGINE_STORAGE, props);;
  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k2, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k3, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k4, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.next_states, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.z_next_states, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.cur_timestep, props->num_models*sizeof(CDATAFORMAT)));

  // Create a local copy of the initial timestep and initialize
  temp_cur_timestep = (CDATAFORMAT*)malloc(props->num_models*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    temp_cur_timestep[i] = props->timestep;

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(bogacki_shampine_mem), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tmem.cur_timestep, temp_cur_timestep, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));

  // Free temporary
  free(temp_cur_timestep);

  return dmem;
  
#else // Used for CPU and OPENMP targets

  bogacki_shampine_mem *mem = (bogacki_shampine_mem*)malloc(sizeof(bogacki_shampine_mem));

  mem->props = props;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k2 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k3 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k4 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->temp = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->next_states = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->z_next_states = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  // Allocate and initialize timesteps
  mem->cur_timestep = (CDATAFORMAT*)malloc(props->num_models*sizeof(CDATAFORMAT));
  for(i=0; i<props->num_models; i++)
    mem->cur_timestep[i] = props->timestep;

  return mem;
#endif
}

__DEVICE__ void SOLVER(bogacki_shampine, pre_eval, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    int *shared_running = (int*)shmem;
    CDATAFORMAT *shared_time = (CDATAFORMAT*)(shared_running + blocksize);
    CDATAFORMAT *shared_states = shared_time + blocksize;

    SOLVER(bogacki_shampine, stage, TARGET, SIMENGINE_STORAGE, mem, shared_states, mem->props->model_states, modelid, threadid, blocksize);

    shared_time[threadid] = mem->props->time[modelid];
    shared_running[threadid] = mem->props->running[modelid];

    __syncthreads();
#endif
    }

__DEVICE__ void SOLVER(bogacki_shampine, post_eval, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    int *shared_running = (int*)shmem;
    CDATAFORMAT *shared_time = (CDATAFORMAT*)(shared_running + blocksize);
    CDATAFORMAT *shared_states = shared_time + blocksize;

    __syncthreads();

    mem->props->running[modelid] = shared_running[threadid];
    mem->props->time[modelid] = shared_time[threadid];

    SOLVER(bogacki_shampine, destage, TARGET, SIMENGINE_STORAGE, mem, mem->props->model_states, shared_states, modelid, threadid, blocksize);
#endif
    }

#if defined TARGET_GPU
__DEVICE__ void SOLVER(bogacki_shampine, stage, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem, CDATAFORMAT *s_states, CDATAFORMAT *g_states, uint modelid, uint threadid, uint blocksize)
    {
    uint i;
    for (i = 0; i < mem->props->statesize; i++)
	{ s_states[threadid + i * blocksize] = g_states[STATE_IDX]; }
    }

__DEVICE__ void SOLVER(bogacki_shampine, destage, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem, CDATAFORMAT *g_states, CDATAFORMAT *s_states, uint modelid, uint threadid, uint blocksize)
    {
    uint i;
    for (i = 0; i < mem->props->statesize; i++)
	{ g_states[STATE_IDX] = s_states[threadid + i * blocksize]; }
    }
#endif


__DEVICE__ int SOLVER(bogacki_shampine, eval, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem, unsigned int modelid, unsigned int threadid) {
  int i;
  int ret;
  uint blocksize = mem->props->gpu.blockx * mem->props->gpu.blocky * mem->props->gpu.blockz;
  uint statesize = mem->props->statesize;

  int appropriate_step = FALSE;

  CDATAFORMAT max_error;

  CDATAFORMAT max_timestep = mem->props->timestep*1024;
  CDATAFORMAT min_timestep = mem->props->timestep/1024;

#if defined TARGET_GPU
  int		*running = (int*)shmem;
  CDATAFORMAT	*time	 = (CDATAFORMAT*)((int*)shmem + blocksize);
  CDATAFORMAT	 t       = time[threadid];

  CDATAFORMAT	*model_states  = time + blocksize;;
  CDATAFORMAT	*k1	       = model_states + blocksize * statesize;
  CDATAFORMAT	*k2	       = k1 + blocksize * statesize;
  CDATAFORMAT	*k3	       = k2 + blocksize * statesize;
  CDATAFORMAT	*k4	       = k3 + blocksize * statesize;
  CDATAFORMAT	*temp	       = k4 + blocksize * statesize;
  CDATAFORMAT	*next_states   = temp + blocksize * statesize;
  CDATAFORMAT	*z_next_states = next_states + blocksize * statesize;

  CDATAFORMAT   *inputs	 = mem->props->inputs;
  CDATAFORMAT   *outputs = mem->props->outputs;
#else
  int		*running = mem->props->running;
  CDATAFORMAT	*time	 = mem->props->time;
  CDATAFORMAT	 t       = time[modelid];

  CDATAFORMAT	*model_states  = mem->props->model_states;
  CDATAFORMAT	*k1	       = mem->k1;
  CDATAFORMAT	*k2	       = mem->k2;
  CDATAFORMAT	*k3	       = mem->k3;
  CDATAFORMAT	*k4	       = mem->k4;
  CDATAFORMAT	*temp	       = mem->temp;
  CDATAFORMAT	*next_states   = mem->next_states;
  CDATAFORMAT	*z_next_states = mem->z_next_states;  

  CDATAFORMAT   *inputs  = mem->props->inputs;
  CDATAFORMAT   *outputs = mem->props->outputs;
#endif


  //fprintf(stderr, "ts=%g\n", mem->cur_timestep[modelid]);

  // Stop the solver if we have reached the stoptime
  running[threadid] = t < mem->props->stoptime;
  if (!running[threadid]) { return 0; }

  ret = model_flows(t, model_states, k1, mem->props->inputs, mem->props->outputs, 1, modelid, threadid, mem->props);

  while(!appropriate_step) {

    //fprintf(stderr, "|-> ts=%g", mem->cur_timestep[modelid]);
    for(i=statesize-1; i>=0; i--) {
      temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
	(mem->cur_timestep[modelid]/FLITERAL(2.0)) * k1[threadid + i * blocksize];
    }

    ret |= model_flows(t+(mem->cur_timestep[modelid]/FLITERAL(2.0)), temp, k2, mem->props->inputs, mem->props->outputs, 0, modelid, threadid, mem->props);

    for(i=statesize-1; i>=0; i--) {
      temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
	(FLITERAL(3.0) * mem->cur_timestep[modelid] / FLITERAL(4.0)) * 
	k2[threadid + i * blocksize];
    }

    ret |= model_flows(t+(FLITERAL(3.0)*mem->cur_timestep[modelid]/FLITERAL(4.0)), temp, k3, mem->props->inputs, mem->props->outputs, 0, modelid, threadid, mem->props);
    
    for(i=statesize-1; i>=0; i--) {
      next_states[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
	(FLITERAL(2.0)/FLITERAL(9.0)) * mem->cur_timestep[modelid] * 
	k1[threadid + i * blocksize] +
	(FLITERAL(1.0)/FLITERAL(3.0)) * mem->cur_timestep[modelid] * 
	k2[threadid + i * blocksize] +
	(FLITERAL(4.0)/FLITERAL(9.0)) * mem->cur_timestep[modelid] * 
	k3[threadid + i * blocksize];
    }
    
    // now compute k4 to adapt the step size
    ret |= model_flows(t+mem->cur_timestep[modelid], next_states, k4, mem->props->inputs, mem->props->outputs, 0, modelid, threadid, mem->props);
    
    for(i=statesize-1; i>=0; i--) {
      z_next_states[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
	(FLITERAL(7.0)/FLITERAL(24.0)) * mem->cur_timestep[modelid] * 
	k1[threadid + i * blocksize] +
	FLITERAL(0.25) * mem->cur_timestep[modelid] * 
	k2[threadid + i * blocksize] +
	(FLITERAL(1.0)/FLITERAL(3.0)) * mem->cur_timestep[modelid] * 
	k3[threadid + i * blocksize] +
	FLITERAL(0.125) * mem->cur_timestep[modelid] * k4[threadid + i * blocksize];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = FLITERAL(-1.0e20);
    CDATAFORMAT max_allowed_error;
    CDATAFORMAT err_sum = 0;
    CDATAFORMAT next_timestep;

    for(i=statesize-1; i>=0; i--) {
      err = fabs(next_states[threadid + i * blocksize] - z_next_states[threadid + i * blocksize]);
      max_allowed_error = mem->props->reltol*fabs(next_states[threadid + i * blocksize])+mem->props->abstol;
      //if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
      
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio > max_error ? ratio : max_error;
      err_sum += ratio * ratio;
      //mexPrintf("%g (%g-%g) ", ratio, next_states[STATE_IDX], z_next_states[STATE_IDX]);

    }
    
    //CDATAFORMAT norm = max_error;
    CDATAFORMAT norm = sqrt(err_sum/mem->props->statesize);
    appropriate_step = norm <= 1;
    if (mem->cur_timestep[modelid] == min_timestep) 
      { appropriate_step = TRUE; }

    if (appropriate_step) { 
      t += mem->cur_timestep[modelid];
      time[threadid] = t;
    }

    next_timestep = FLITERAL(0.90) * mem->cur_timestep[modelid] * pow(FLITERAL(1.0)/norm, FLITERAL(1.0)/FLITERAL(3.0));
#if defined __DEVICE_EMULATION__
    //fprintf(stderr,"model: %d ts: %g -> %g (norm=%g)\n", modelid, mem->cur_timestep[modelid], next_timestep, norm);
#endif

    // Try to hit the stoptime exactly
    if (next_timestep > mem->props->stoptime - t)
      mem->cur_timestep[modelid] = mem->props->stoptime - t;
    else if ((isnan(next_timestep)) || (next_timestep < min_timestep))
      mem->cur_timestep[modelid] = min_timestep;
    else if (next_timestep > max_timestep )
      mem->cur_timestep[modelid] = max_timestep;
    else
      mem->cur_timestep[modelid] = next_timestep;

  }

  // just return back the expected
  for(i=statesize-1; i>=0; i--) {
    model_states[threadid] = next_states[threadid];
  }
  
  return ret;
}

void SOLVER(bogacki_shampine, free, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem) {
#if defined TARGET_GPU
  bogacki_shampine_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(bogacki_shampine_mem), cudaMemcpyDeviceToHost));

  GPU_ENTRY(free_props, SIMENGINE_STORAGE, tmem.props);

  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(tmem.k2));
  cutilSafeCall(cudaFree(tmem.k3));
  cutilSafeCall(cudaFree(tmem.k4));
  cutilSafeCall(cudaFree(tmem.temp));
  cutilSafeCall(cudaFree(tmem.next_states));
  cutilSafeCall(cudaFree(tmem.z_next_states));
  cutilSafeCall(cudaFree(tmem.cur_timestep));
  cutilSafeCall(cudaFree(mem));

  GPU_ENTRY(exit, SIMENGINE_STORAGE);

#else // Used for CPU and OPENMP targets

  free(mem->k1);
  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->temp);
  free(mem->next_states);
  free(mem->z_next_states);
  free(mem->cur_timestep);
  free(mem);
#endif
}
