#include"solvers.h"

void GPU_ENTRY(init, SIMENGINE_STORAGE){
  // FIXME Add more checking of capabilities and devices available!
  cudaSetDevice(cutGetMaxGflopsDeviceId());
  cudaSetDeviceFlags(cudaDeviceMapHost);
}

void GPU_ENTRY(exit, SIMENGINE_STORAGE){
  cudaThreadExit();
}

// Takes a solver_props pointer on the CPU and returns a pointer to a mirrored structure on the GPU
solver_props *GPU_ENTRY(init_props, SIMENGINE_STORAGE, solver_props *props){
  // Local temp
  solver_props tprops;

  // GPU datastructures
  solver_props *dprops;

  void *ob;

  //  cutilSafeCall(cudaMalloc((void**)&tprops.ob, props->ob_size));
  cutilSafeCall(cudaHostAlloc(&ob, props->ob_size, cudaHostAllocMapped | cudaHostAllocPortable));
  memcpy(ob, props->ob, props->ob_size);
  props->gpu.ob = ob;

  

  // Copy the properties to local temporary
  memcpy(&tprops, props, sizeof(solver_props));

  // Allocate GPU space for props and all pointer fields of props
  cutilSafeCall(cudaMalloc((void**)&dprops, sizeof(solver_props)));
  cutilSafeCall(cudaMalloc((void**)&tprops.time, props->num_models*sizeof(CDATAFORMAT)));
  if (props->statesize) {
    cutilSafeCall(cudaMalloc((void**)&tprops.model_states, props->num_models*props->statesize*sizeof(CDATAFORMAT)));
  }
  else { tprops.model_states = 0; }

  if (props->inputsize) {
    cutilSafeCall(cudaMalloc((void**)&tprops.inputs, props->num_models*props->inputsize*sizeof(CDATAFORMAT)));
  }
  else { tprops.inputs = 0; }

  if (props->outputsize) {
    cutilSafeCall(cudaMalloc((void**)&tprops.outputs, props->num_models*props->outputsize*sizeof(CDATAFORMAT)));
  }
  else { tprops.outputs = 0; }

  if (0 != cutilSafeCall(cudaHostGetDevicePointer(&tprops.ob, ob, 0))) {
      return 0;
      }

  // Copy props to GPU
  cutilSafeCall(cudaMemcpy(dprops, &tprops, sizeof(solver_props), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tprops.time, props->time, props->num_models*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tprops.model_states, props->model_states, props->num_models*props->statesize*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(tprops.inputs, props->inputs, props->num_models*props->inputsize*sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));


  // Pointers to GPU memory for data we need to be able to retrieve
  props->gpu.time = tprops.time;
  props->gpu.model_states = tprops.model_states;
  return dprops;
}

// Frees a GPU solver props structure
void GPU_ENTRY(free_props, SIMENGINE_STORAGE, solver_props *props){
  solver_props tprops;

  cutilSafeCall(cudaMemcpy(&tprops, props, sizeof(solver_props), cudaMemcpyDeviceToHost));

  if (tprops.time)
    { cutilSafeCall(cudaFree(tprops.time)); }
  if (tprops.model_states)
    { cutilSafeCall(cudaFree(tprops.model_states)); }
  if (tprops.inputs)
    { cutilSafeCall(cudaFree(tprops.inputs)); }
  if (tprops.outputs)
    { cutilSafeCall(cudaFree(tprops.outputs)); }
  if (tprops.running)
    { cutilSafeCall(cudaFree(tprops.running)); }
  if (props)
    { cutilSafeCall(cudaFree(props)); }
}
