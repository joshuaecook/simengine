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

/*// No need to alloc these when using shared memory
  cutilSafeCall(cudaMalloc((void**)&tmem.k2, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k3, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.k4, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*props->num_models*sizeof(CDATAFORMAT)));
*/

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

__DEVICE__ void SOLVER(rk4, stage, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    CDATAFORMAT *global_states = mem->props->model_states;
    CDATAFORMAT *shared_states = shmem;
    CDATAFORMAT *shared_time = shmem + 6 * blocksize * mem->props->statesize;

    for (int i = 0; i < mem->props->statesize; i++)
	{
	shared_states[threadid + i * blocksize] = global_states[STATE_IDX];
	}

    shared_time[threadid] = mem->props->time[modelid];

    __syncthreads();
#endif
    }

__DEVICE__ void SOLVER(rk4, destage, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, uint modelid, uint threadid, uint blocksize)
    {
#if defined TARGET_GPU
    CDATAFORMAT *global_states = mem->props->model_states;
    CDATAFORMAT *shared_states = shmem;
    CDATAFORMAT *shared_time = shmem + 6 * blocksize * mem->props->statesize;

    __syncthreads();

//    mem->props->time[modelid] = shared_time[threadid];

    for (int i = 0; i < mem->props->statesize; i++)
	{
	global_states[STATE_IDX] = shared_states[threadid + i * blocksize];
	}
#endif
    }

__DEVICE__ int SOLVER(rk4, eval, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, unsigned int modelid, unsigned int threadid) {
  int i;
  int ret;
  uint blocksize = mem->props->gpu.blockx * mem->props->gpu.blocky * mem->props->gpu.blockz;
  uint statesize = mem->props->statesize;
  CDATAFORMAT timestep = mem->props->timestep;

#if defined TARGET_GPU
  CDATAFORMAT	*model_states = shmem;

  CDATAFORMAT	*k1	      = model_states + blocksize * statesize;
  CDATAFORMAT	*k2	      = k1 + blocksize * statesize;
  CDATAFORMAT	*k3	      = k2 + blocksize * statesize;
  CDATAFORMAT	*k4	      = k3 + blocksize * statesize;
  CDATAFORMAT	*temp	      = k4 + blocksize * statesize;

  CDATAFORMAT	*time	      = mem->props->time;
//  CDATAFORMAT	*time	      = temp + blocksize * statesize;
  CDATAFORMAT   *inputs       = mem->props->inputs;
  CDATAFORMAT   *outputs      = mem->props->outputs;

  int		*running      = mem->props->running;
#else
  CDATAFORMAT	*model_states = mem->props->model_states;

  CDATAFORMAT	*k1	      = mem->k1;
  CDATAFORMAT	*k2	      = mem->k2;
  CDATAFORMAT	*k3	      = mem->k3;
  CDATAFORMAT	*k4	      = mem->k4;
  CDATAFORMAT	*temp	      = mem->temp;

  CDATAFORMAT	*time	      = mem->props->time;
  CDATAFORMAT   *inputs       = mem->props->inputs;
  CDATAFORMAT   *outputs      = mem->props->outputs;

  int		*running      = mem->props->running;
#endif


  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  running[modelid] = time[modelid] + timestep <= mem->props->stoptime;
  if(!running[modelid]) { return 0; }

  ret = model_flows(time[modelid], model_states, k1, inputs, outputs, 1, modelid, threadid, mem->props);
  for(i=statesize-1; i>=0; i--) {
    temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
      (timestep/2) * k1[threadid + i * blocksize];
  }
  ret |= model_flows(time[modelid]+(timestep/2), temp, k2, inputs, outputs, 0, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
      (timestep/2) * k2[threadid + i * blocksize];
  }
  ret |= model_flows(time[modelid]+(timestep/2), temp, k3, inputs, outputs, 0, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    temp[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
	timestep * k3[threadid + i * blocksize];
  }
  ret |= model_flows(time[modelid] + timestep, temp, k4, inputs, outputs, 0, modelid, threadid, mem->props);

  for(i=statesize-1; i>=0; i--) {
    model_states[threadid + i * blocksize] = model_states[threadid + i * blocksize] +
        (timestep/FLITERAL(6.0)) * 
	(1 * k1[threadid + i * blocksize] +
	 2 * k2[threadid + i * blocksize] +
	 2 * k3[threadid + i * blocksize] +
	 1 * k4[threadid + i * blocksize]);
  }

  time[modelid] += timestep;

  return ret;
}

void SOLVER(rk4, free, TARGET, SIMENGINE_STORAGE, rk4_mem *mem) {
#if defined TARGET_GPU
  rk4_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, mem, sizeof(rk4_mem), cudaMemcpyDeviceToHost));

  GPU_ENTRY(free_props, SIMENGINE_STORAGE, tmem.props);


  cutilSafeCall(cudaFree(tmem.k1));
/*
  cutilSafeCall(cudaFree(tmem.k2));
  cutilSafeCall(cudaFree(tmem.k3));
  cutilSafeCall(cudaFree(tmem.k4));
  cutilSafeCall(cudaFree(tmem.temp));
*/
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
