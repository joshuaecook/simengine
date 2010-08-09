// Heun Integration Method
// Copyright 2010 Simatra Modeling Technologies, L.L.C.

/*
3)  Heun method (predictor-corrector type Euler)

y[n+1]* = y[n] + h*f(y[n])
y[n+1] = y[n] + (h/2)*(f(y[n]) + f(y[n+1]*))

If t (or whatever variable the DEQ is in respect to) is part of the
equation, i.e. dy/dt = f(y,t), the equations would be (note, we
probably need to consider implementing all methods for the general
case dy/dt = f(y, t), if we can):

y[n+1]* = y[n] + h*f(y[n], t[n])
y[n+1] = y[n] + (h/2)*(f(y[n], t[n]) + f(y[n+1]*, t[n+1]))

So, you're estimating y[n+1] using Euler, then calculating what the
derivative would be using that estimate, and using an average of the
two derivatives to calculate the "actual" y[n+1]

  We break the equations into the following parts:
    y.base = f(y[n],t[n])
    y.predict = f(y[n] + h * y.base)
    answer = y[n] + (h/2) * (y.base + y.predict)
*/



typedef struct {
  CDATAFORMAT *base;
  CDATAFORMAT *temp;
  CDATAFORMAT *predictor;
} heun_mem;

__HOST__
int heun_init(solver_props *props){
#if defined TARGET_GPU
  // Temporary CPU copies of GPU datastructures
  heun_mem tmem;
  // GPU datastructures
  heun_mem *dmem;
  
  // Allocate GPU space for mem and pointer fields of mem (other than props)
  cutilSafeCall(cudaMalloc((void**)&dmem, sizeof(heun_mem)));
  props->mem = dmem;
  cutilSafeCall(cudaMalloc((void**)&tmem.base, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.temp, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));
  cutilSafeCall(cudaMalloc((void**)&tmem.predictor, props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT)));

  // Copy mem structure to GPU
  cutilSafeCall(cudaMemcpy(dmem, &tmem, sizeof(heun_mem), cudaMemcpyHostToDevice));

#else // Used for CPU and OPENMP targets

  heun_mem *mem = (heun_mem*)malloc(sizeof(heun_mem));

  props->mem = mem;
  mem->temp = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
  mem->base = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
  mem->predictor = (CDATAFORMAT*)malloc(props->statesize*PARALLEL_MODELS*sizeof(CDATAFORMAT));
#endif

  return 0;
}

__DEVICE__
int heun_eval(solver_props *props, unsigned int modelid){
  int i;
  int ret;

  heun_mem *mem = (heun_mem*)props->mem;

  ret = model_flows(props->time[modelid], props->model_states, mem->base, props, 1, modelid);

  for(i=props->statesize-1; i>=0; i--) {
    mem->temp[STATE_IDX] = props->model_states[STATE_IDX] +
      props->timestep*mem->base[STATE_IDX];
  }  

  ret |= model_flows(props->time[modelid]+(props->timestep/2), mem->temp, mem->predictor, props, 0, modelid);

  for(i=props->statesize-1; i>=0; i--) {
    props->next_states[STATE_IDX] = props->model_states[STATE_IDX] + (props->timestep/2) * (mem->base[STATE_IDX]+mem->predictor[STATE_IDX]);
  }

  props->next_time[modelid] += props->timestep;

  return ret;
}

__HOST__
int heun_free(solver_props *props){
#if defined TARGET_GPU
  heun_mem *dmem = (heun_mem*)props->mem;
  heun_mem tmem;

  cutilSafeCall(cudaMemcpy(&tmem, dmem, sizeof(heun_mem), cudaMemcpyDeviceToHost));

  cutilSafeCall(cudaFree(tmem.base));
  cutilSafeCall(cudaFree(tmem.temp));
  cutilSafeCall(cudaFree(tmem.predictor));
  cutilSafeCall(cudaFree(dmem));

#else // Used for CPU and OPENMP targets

  heun_mem *mem =(heun_mem*)props->mem;

  free(mem->temp);
  free(mem->base);
  free(mem->predictor);
  free(mem);
#endif // defined TARGET_GPU  free(mem->k1);

  return 0;
}
