// Forward Euler Integration Method
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
#include "solvers.h"

#if defined TARGET_GPU
extern __shared__ CDATAFORMAT shmem[];
#endif

forwardeuler_mem *SOLVER(forwardeuler, init, TARGET, SIMENGINE_STORAGE, solver_props *props) {
#if defined TARGET_GPU
  GPU_ENTRY(init, SIMENGINE_STORAGE);

  // Temporary CPU copies of GPU datastructures
  forwardeuler_mem tmem;
  // GPU datastructures
  forwardeuler_mem *dmem;

  // Computes GPU kernel geometry
  size_t shmem_per_thread, total_shmem = 1<<14;
  int warp_size = 1<<5;
  uint threads_per_block;
  uint num_gpu_threads;
  uint num_gpu_blocks;

  // shared space for model states and solver overhead
  shmem_per_thread = sizeof(CDATAFORMAT) * props->statesize * 2;
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
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(forwardeuler_mem)));
  tmem.props = GPU_ENTRY(init_props, SIMENGINE_STORAGE, props);
  if (!tmem.props)
      { return 0; }
  cutilSafeCall(cudaMalloc((void**)&tmem.k1, props->statesize*props->num_models*sizeof(CDATAFORMAT)));

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(forwardeuler_mem), cudaMemcpyHostToDevice));

  return dmem;
  
#else // Used for CPU and OPENMP targets

  forwardeuler_mem *mem = (forwardeuler_mem*)malloc(sizeof(forwardeuler_mem));

  mem->props = props;
  mem->k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  return mem;

#endif // defined TARGET_GPU
}

__DEVICE__ void SOLVER(forwardeuler, stage, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    CDATAFORMAT *global_states = mem->props->model_states;
    CDATAFORMAT *shared_states = shmem;

    for (int i = 0; i < mem->props->statesize; i++)
	{
	shared_states[threadid + i * blocksize] = global_states[STATE_IDX];
	}

    __syncthreads();
#endif
    }

__DEVICE__ void SOLVER(forwardeuler, destage, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    CDATAFORMAT *global_states = mem->props->model_states;
    CDATAFORMAT *shared_states = shmem;

    __syncthreads();

    for (int i = 0; i < mem->props->statesize; i++)
	{
	global_states[STATE_IDX] = shared_states[threadid + i * blocksize];
	}
#endif
    }

__DEVICE__ int SOLVER(forwardeuler, eval, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem, uint modelid, uint threadid) {
  int ret, i;
  uint blocksize = mem->props->gpu.blockx * mem->props->gpu.blocky * mem->props->gpu.blockz;
  uint statesize = mem->props->statesize;
  CDATAFORMAT timestep = mem->props->timestep;

#if defined TARGET_GPU
  CDATAFORMAT	*model_states = shmem;
  CDATAFORMAT	*k1	      = model_states + blocksize * statesize;
#else
  CDATAFORMAT	*model_states = mem->props->model_states;
  CDATAFORMAT	*k1	      = mem->k1;
#endif
  CDATAFORMAT	*time	      = mem->props->time;
  CDATAFORMAT   *inputs       = mem->props->inputs;
  CDATAFORMAT   *outputs      = mem->props->outputs;

  int		*running      = mem->props->running;

  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  running[modelid] = time[modelid] + timestep <= mem->props->stoptime;
  if(!running[modelid]) { return 0; }

  ret = model_flows(time[modelid], model_states, k1, inputs, outputs, 1, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    model_states[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
      timestep * k1[threadid + i * blocksize];
  }

  time[modelid] += timestep;

  return ret;
}

void SOLVER(forwardeuler, free, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem) {
#if defined TARGET_GPU
  forwardeuler_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(forwardeuler_mem), cudaMemcpyDeviceToHost));

  GPU_ENTRY(free_props, SIMENGINE_STORAGE, tmem.props);

  cutilSafeCall(cudaFree(tmem.k1));
  cutilSafeCall(cudaFree(mem));

  GPU_ENTRY(exit, SIMENGINE_STORAGE);

#else // Used for CPU and OPENMP targets

  free(mem->k1);
  free(mem);

#endif // defined TARGET_GPU
}
