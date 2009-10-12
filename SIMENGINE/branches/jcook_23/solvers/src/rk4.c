// Runga-Kutta (4th order) Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"

#if defined TARGET_GPU
extern __shared__ CDATAFORMAT shmem[];
#endif

rk4_mem *SOLVER(rk4, init, TARGET, SIMENGINE_STORAGE, solver_props *props) {
#if defined TARGET_GPU
  GPU_ENTRY(init, SIMENGINE_STORAGE);

  // Temporary CPU copies of GPU datastructures
  rk4_mem tmem;
  // GPU datastructures
  rk4_mem *dmem;

  // Computes GPU kernel geometry
  size_t shmem_per_thread, total_shmem = 1<<14;
  int warp_size = 1<<5;
  uint threads_per_block;
  uint num_gpu_threads;
  uint num_gpu_blocks;

  // shared space for model states and solver overhead
  shmem_per_thread = sizeof(CDATAFORMAT) * props->statesize * 6; // 6 = magic for rk4
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
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(rk4_mem)));
  tmem.props = GPU_ENTRY(init_props, SIMENGINE_STORAGE, props);

  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*props->num_models*sizeof(CDATAFORMAT)));

  cutilSafeCall(cudaMalloc((void**)&tmem.k2, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k3, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k4, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*props->num_models*sizeof(CDATAFORMAT)));

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(rk4_mem), cudaMemcpyHostToDevice));

  return dmem;
  
#else // Used for CPU and OPENMP targets

  rk4_mem *mem = (rk4_mem*)malloc(sizeof(rk4_mem));

  mem->props = props;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k2 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k3 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->k4 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));
  mem->temp = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return mem;
#endif
}

__DEVICE__ void SOLVER(rk4, pre_eval, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    int *shared_running = (int*)shmem;
    CDATAFORMAT *shared_time = (CDATAFORMAT*)(shared_running + blocksize);
    CDATAFORMAT *shared_states = shared_time + blocksize;

    SOLVER(rk4, stage, TARGET, SIMENGINE_STORAGE, mem, shared_states, mem->props->model_states, modelid, threadid, blocksize);

    shared_time[threadid] = mem->props->time[modelid];
    shared_running[threadid] = mem->props->running[modelid];

    __syncthreads();
#endif
    }

__DEVICE__ void SOLVER(rk4, post_eval, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    int *shared_running = (int*)shmem;
    CDATAFORMAT *shared_time = (CDATAFORMAT*)(shared_running + blocksize);
    CDATAFORMAT *shared_states = shared_time + blocksize;

    __syncthreads();

    mem->props->running[modelid] = shared_running[threadid];
    mem->props->time[modelid] = shared_time[threadid];

    SOLVER(rk4, destage, TARGET, SIMENGINE_STORAGE, mem, mem->props->model_states, shared_states, modelid, threadid, blocksize);
#endif
    }

#if defined TARGET_GPU
__DEVICE__ void SOLVER(rk4, stage, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, CDATAFORMAT *s_states, CDATAFORMAT *g_states, uint modelid, uint threadid, uint blocksize)
    {
    uint i;
    for (i = 0; i < mem->props->statesize; i++)
	{ s_states[threadid + i * blocksize] = g_states[STATE_IDX]; }
    }

__DEVICE__ void SOLVER(rk4, destage, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, CDATAFORMAT *g_states, CDATAFORMAT *s_states, uint modelid, uint threadid, uint blocksize)
    {
    uint i;
    for (i = 0; i < mem->props->statesize; i++)
	{ g_states[STATE_IDX] = s_states[threadid + i * blocksize]; }
    }
#endif


__DEVICE__ int SOLVER(rk4, eval, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, unsigned int modelid, unsigned int threadid) {
  int i;
  int ret;
  uint blocksize = mem->props->gpu.blockx * mem->props->gpu.blocky * mem->props->gpu.blockz;
  uint statesize = mem->props->statesize;
  CDATAFORMAT dt = mem->props->timestep;

#if defined TARGET_GPU
  int		*running = (int*)shmem;
  CDATAFORMAT	*time	 = (CDATAFORMAT*)((int*)shmem + blocksize);
  CDATAFORMAT   t        = time[threadid];

  CDATAFORMAT	*model_states = time + blocksize;;

  CDATAFORMAT	*k1   = model_states + blocksize * statesize;
  CDATAFORMAT	*k2   = k1 + blocksize * statesize;
  CDATAFORMAT	*k3   = k2 + blocksize * statesize;
  CDATAFORMAT	*k4   = k3 + blocksize * statesize;
  CDATAFORMAT	*temp = k4 + blocksize * statesize;

  CDATAFORMAT   *inputs	 = mem->props->inputs;
  CDATAFORMAT   *outputs = mem->props->outputs;

#else
  CDATAFORMAT	*model_states = mem->props->model_states;

  CDATAFORMAT	*k1	      = mem->k1;
  CDATAFORMAT	*k2	      = mem->k2;
  CDATAFORMAT	*k3	      = mem->k3;
  CDATAFORMAT	*k4	      = mem->k4;
  CDATAFORMAT	*temp	      = mem->temp;

  CDATAFORMAT	*time	      = mem->props->time;
  CDATAFORMAT   t             = time[modelid];
  CDATAFORMAT   *inputs       = mem->props->inputs;
  CDATAFORMAT   *outputs      = mem->props->outputs;

  int		*running      = mem->props->running;
#endif


  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  running[threadid] = t + dt <= mem->props->stoptime;
  if(!running[threadid]) { return 0; }

  ret = model_flows(t, model_states, k1, inputs, outputs, 1, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
      (dt/FLITERAL(2.0)) * k1[threadid + i * blocksize];
  }

  ret |= model_flows(t+(dt/FLITERAL(2.0)), temp, k2, inputs, outputs, 0, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
      (dt/FLITERAL(2.0)) * k2[threadid + i * blocksize];
  }

  ret |= model_flows(t+(dt/FLITERAL(2.0)), temp, k3, inputs, outputs, 0, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
	dt * k3[threadid + i * blocksize];
  }

  ret |= model_flows(t + dt, temp, k4, inputs, outputs, 0, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    model_states[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
      (dt/FLITERAL(6.0)) * 
      (FLITERAL(1.0) * k1[threadid + i * blocksize] +
       FLITERAL(2.0) * k2[threadid + i * blocksize] +
       FLITERAL(2.0) * k3[threadid + i * blocksize] +
       FLITERAL(1.0) * k4[threadid + i * blocksize]);
  }

  time[threadid] += dt;

  return ret;
}

void SOLVER(rk4, free, TARGET, SIMENGINE_STORAGE, rk4_mem *mem) {
#if defined TARGET_GPU
  rk4_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(rk4_mem), cudaMemcpyDeviceToHost));

  GPU_ENTRY(free_props, SIMENGINE_STORAGE, tmem.props);


  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(tmem.k2));
  cutilSafeCall(cudaFree(tmem.k3));
  cutilSafeCall(cudaFree(tmem.k4));
  cutilSafeCall(cudaFree(tmem.temp));
  cutilSafeCall(cudaFree(mem));

  GPU_ENTRY(exit, SIMENGINE_STORAGE);

#else // Used for CPU and OPENMP targets

  free(mem->k2);
  free(mem->k3);
  free(mem->k4);
  free(mem->temp);
  free(mem);

#endif // defined TARGET_GPU  free(mem->k1);
}
