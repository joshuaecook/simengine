// C Execution Engine for top-level model: stg_spiker
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

#include "stg_spiker.h"
#ifdef TARGET_GPU
#include "solvers/dormand_prince.cu"
#endif

#define BLOCK_SIZE 192

static output_buffer OB;
__DEVICE__ float4 *VB;
__DEVICE__ float4 *CB;

float4 *dclut = 0;
__DEVICE__ float4 *CLUT;
__DEVICE__ unsigned int CLUT_LENGTH;

int log_outputs(output_buffer *, simengine_output *, unsigned int);

__DEVICE__ void init_output_buffer(output_buffer *ob, unsigned int modelid){
  ob->full[modelid] = 0;
  ob->count[modelid] = 0;
  ob->vb_count[modelid] = 0;
  ob->ptr[modelid] = &ob->buffer[modelid*OUTPUT_BUFFER_LENGTH];
  ob->end[modelid] = &ob->buffer[(modelid+1)*OUTPUT_BUFFER_LENGTH];
}

__DEVICE__ void buffer_outputs(double t, double t0, double t1, output_data *od, output_buffer *ob, unsigned int modelid) {
  
  { // Generating output for symbol Vm
    int cond = 1;
    if (cond) {
      if (0 != VB) {
	float z = 2.0f * modelid / NUM_MODELS - 1.0f;
	float y = od->mdlvar__Vm / 120.0f;
	float x = t;//(t - t0) / (t1 - t0) * 2.0f - 1.0f;

#if defined __DEVICE_EMULATION__
	PRINTF("VB[%d * %d + %d = %d] = (%.4f, %.4f, %.4f)\n",
	       modelid, MAX_ITERATIONS, ob->vb_count[modelid],
	       SA_IDX(NUM_MODELS, MAX_ITERATIONS, modelid, ob->vb_count[modelid]),
	       x, y, z);
#endif

	int cidx = MIN(CLUT_LENGTH,MAX(0,CLUT_LENGTH * (y + 1.0f / 2.0f)));
	/*
#if defined __DEVICE_EMULATION__
	PRINTF("CLUT[%.4f = %d]\n", y, cidx);
#endif
	*/
	// float r = (y + 1.0f) / 2.0f;
	// float g = 0.0f;//(y + 1.0f) / 2.0f;
	// float b = 0.18f;

	VB[SA_IDX(NUM_MODELS, MAX_ITERATIONS, modelid, ob->vb_count[modelid])] = 
	  make_float4(x,y,z,1.0f);

	CB[SA_IDX(NUM_MODELS, MAX_ITERATIONS, modelid, ob->vb_count[modelid])] = 
	  CLUT[cidx];
	//make_float4(r,g,b,0.82f);

	++(ob->vb_count[modelid]);
      }


      ((unsigned int*)(ob->ptr[modelid]))[0] = 0;
      ((unsigned int*)(ob->ptr[modelid]))[1] = 2;
      ob->ptr[modelid] = &((unsigned int*)(ob->ptr[modelid]))[2];
      *((CDATAFORMAT*)(ob->ptr[modelid])) = t;
      ob->ptr[modelid] = &((CDATAFORMAT*)(ob->ptr[modelid]))[1];
      *((CDATAFORMAT*)(ob->ptr[modelid])) = od->mdlvar__Vm;
      ob->ptr[modelid] = &((CDATAFORMAT*)(ob->ptr[modelid]))[1];
      ob->count[modelid]++;
      ob->full[modelid] = MAX_OUTPUT_SIZE > ((unsigned long long)(ob->end[modelid]) - (unsigned long long)(ob->ptr[modelid]));

    }
  }
  
}
// Flow code function declarations
__DEVICE__ int flow_stg_spiker(CDATAFORMAT t, const struct statedata_stg_spiker *y, struct statedata_stg_spiker *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid);

__DEVICE__ int flow_stg_spiker(CDATAFORMAT t, const struct statedata_stg_spiker *y, struct statedata_stg_spiker *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid) {
  
  // mapping inputs to variables
  CDATAFORMAT mdlvar__gNa = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 0, modelid)];
  CDATAFORMAT mdlvar__gCaT = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 1, modelid)];
  CDATAFORMAT mdlvar__gCaS = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 2, modelid)];
  CDATAFORMAT mdlvar__gA = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 3, modelid)];
  CDATAFORMAT mdlvar__gKCa = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 4, modelid)];
  CDATAFORMAT mdlvar__gKd = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 5, modelid)];
  CDATAFORMAT mdlvar__gh = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 6, modelid)];
  CDATAFORMAT mdlvar__gleak = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, 7, modelid)];
  
  // writing all intermediate, instance, and differential equation expressions
  CDATAFORMAT mdlvar__Vm = y[STRUCT_IDX].mdlvar__V[ARRAY_IDX];
  CDATAFORMAT mdlvar__ECa = FLITERAL(0.12193595E2)*log((FLITERAL(0.3E4)/y[STRUCT_IDX].mdlvar__Caconc[ARRAY_IDX]));
  CDATAFORMAT mdlvar__INa = mdlvar__gNa*y[STRUCT_IDX].mdlvar__mNa[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mNa[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mNa[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__hNa[ARRAY_IDX]*(mdlvar__Vm-FLITERAL(0.5E2))*FLITERAL(0.6283E-3);
  CDATAFORMAT mdlvar__ICaT = mdlvar__gCaT*y[STRUCT_IDX].mdlvar__mCaT[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mCaT[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mCaT[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__hCaT[ARRAY_IDX]*(mdlvar__Vm-mdlvar__ECa)*FLITERAL(0.6283E-3);
  CDATAFORMAT mdlvar__ICaS = mdlvar__gCaS*y[STRUCT_IDX].mdlvar__mCaS[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mCaS[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mCaS[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__hCaS[ARRAY_IDX]*(mdlvar__Vm-mdlvar__ECa)*FLITERAL(0.6283E-3);
  CDATAFORMAT mdlvar__IA = mdlvar__gA*y[STRUCT_IDX].mdlvar__mA[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mA[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mA[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__hA[ARRAY_IDX]*(mdlvar__Vm-FLITERAL(-0.8E2))*FLITERAL(0.6283E-3);
  CDATAFORMAT mdlvar__IKCa = mdlvar__gKCa*y[STRUCT_IDX].mdlvar__mKCa[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mKCa[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mKCa[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mKCa[ARRAY_IDX]*(mdlvar__Vm-FLITERAL(-0.8E2))*FLITERAL(0.6283E-3);
  CDATAFORMAT mdlvar__IKd = mdlvar__gKd*y[STRUCT_IDX].mdlvar__mKd[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mKd[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mKd[ARRAY_IDX]*y[STRUCT_IDX].mdlvar__mKd[ARRAY_IDX]*(mdlvar__Vm-FLITERAL(-0.8E2))*FLITERAL(0.6283E-3);
  CDATAFORMAT mdlvar__Ih = mdlvar__gh*y[STRUCT_IDX].mdlvar__mh[ARRAY_IDX]*(mdlvar__Vm-FLITERAL(-0.2E2))*FLITERAL(0.6283E-3);
  CDATAFORMAT mdlvar__Ileak = mdlvar__gleak*(mdlvar__Vm-FLITERAL(-0.5E2))*FLITERAL(0.6283E-3);
  dydt[STRUCT_IDX].mdlvar__V[ARRAY_IDX] = FLITERAL(0.15915963711602737E4)*((((((((-(mdlvar__INa))-mdlvar__ICaT)-mdlvar__ICaS)-mdlvar__IA)-mdlvar__IKCa)-mdlvar__IKd)-mdlvar__Ih)-mdlvar__Ileak);
  dydt[STRUCT_IDX].mdlvar__mNa[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.255E2))/FLITERAL(-0.529E1))))-y[STRUCT_IDX].mdlvar__mNa[ARRAY_IDX])/(FLITERAL(0.264E1)+FLITERAL(-0.252E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.12E3))/FLITERAL(-0.25E2)))));
  dydt[STRUCT_IDX].mdlvar__hNa[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.489E2))/FLITERAL(0.518E1))))-y[STRUCT_IDX].mdlvar__hNa[ARRAY_IDX])/((FLITERAL(0.134E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.629E2))/FLITERAL(-0.1E2)))))*(FLITERAL(0.15E1)+FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.349E2))/FLITERAL(0.36E1))))));
  dydt[STRUCT_IDX].mdlvar__mCaT[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.271E2))/FLITERAL(-0.72E1))))-y[STRUCT_IDX].mdlvar__mCaT[ARRAY_IDX])/(FLITERAL(0.434E2)+FLITERAL(-0.426E2)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.681E2))/FLITERAL(-0.205E2)))));
  dydt[STRUCT_IDX].mdlvar__hCaT[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.321E2))/FLITERAL(0.55E1))))-y[STRUCT_IDX].mdlvar__hCaT[ARRAY_IDX])/(FLITERAL(0.21E3)+FLITERAL(-0.1796E3)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.55E2))/FLITERAL(-0.169E2)))));
  dydt[STRUCT_IDX].mdlvar__mCaS[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.33E2))/FLITERAL(-0.81E1))))-y[STRUCT_IDX].mdlvar__mCaS[ARRAY_IDX])/(FLITERAL(0.28E1)+FLITERAL(0.14E2)/(exp(((mdlvar__Vm+FLITERAL(0.27E2))/FLITERAL(0.1E2)))+exp(((mdlvar__Vm+FLITERAL(0.7E2))/FLITERAL(-0.13E2)))));
  dydt[STRUCT_IDX].mdlvar__hCaS[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.6E2))/FLITERAL(0.62E1))))-y[STRUCT_IDX].mdlvar__hCaS[ARRAY_IDX])/(FLITERAL(0.12E3)+FLITERAL(0.3E3)/(exp(((mdlvar__Vm+FLITERAL(0.55E2))/FLITERAL(0.9E1)))+exp(((mdlvar__Vm+FLITERAL(0.65E2))/FLITERAL(-0.16E2)))));
  dydt[STRUCT_IDX].mdlvar__mA[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.272E2))/FLITERAL(-0.87E1))))-y[STRUCT_IDX].mdlvar__mA[ARRAY_IDX])/(FLITERAL(0.232E2)+FLITERAL(-0.208E2)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.329E2))/FLITERAL(-0.152E2)))));
  dydt[STRUCT_IDX].mdlvar__hA[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.569E2))/FLITERAL(0.49E1))))-y[STRUCT_IDX].mdlvar__hA[ARRAY_IDX])/(FLITERAL(0.772E2)+FLITERAL(-0.584E2)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.389E2))/FLITERAL(-0.265E2)))));
  dydt[STRUCT_IDX].mdlvar__mKCa[ARRAY_IDX] = ((y[STRUCT_IDX].mdlvar__Caconc[ARRAY_IDX]/(y[STRUCT_IDX].mdlvar__Caconc[ARRAY_IDX]+FLITERAL(0.3E1)))*(FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.283E2))/FLITERAL(-0.126E2)))))-y[STRUCT_IDX].mdlvar__mKCa[ARRAY_IDX])/(FLITERAL(0.1806E3)+FLITERAL(-0.1502E3)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.46E2))/FLITERAL(-0.227E2)))));
  dydt[STRUCT_IDX].mdlvar__mKd[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.123E2))/FLITERAL(-0.118E2))))-y[STRUCT_IDX].mdlvar__mKd[ARRAY_IDX])/(FLITERAL(0.144E2)+FLITERAL(-0.128E2)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.283E2))/FLITERAL(-0.192E2)))));
  dydt[STRUCT_IDX].mdlvar__mh[ARRAY_IDX] = (FLITERAL(0.1E1)/(FLITERAL(0.1E1)+exp(((mdlvar__Vm+FLITERAL(0.75E2))/FLITERAL(0.55E1))))-y[STRUCT_IDX].mdlvar__mh[ARRAY_IDX])/(FLITERAL(0.2E1)/(exp(((mdlvar__Vm+FLITERAL(0.1697E3))/FLITERAL(-0.116E2)))+exp(((mdlvar__Vm-FLITERAL(0.267E2))/FLITERAL(0.143E2)))));
  dydt[STRUCT_IDX].mdlvar__Caconc[ARRAY_IDX] = FLITERAL(0.5E-2)*((FLITERAL(-0.1496E5)*(mdlvar__ICaT+mdlvar__ICaS)-y[STRUCT_IDX].mdlvar__Caconc[ARRAY_IDX])+FLITERAL(0.5E-1));
  
  // writing output variables
  if (first_iteration) {
    output_data *od = (output_data*)outputs;
    od[modelid].mdlvar__Vm = mdlvar__Vm;
  }
  
  return 0;
}


__DEVICE__ int model_flows(CDATAFORMAT t, const CDATAFORMAT *y, CDATAFORMAT *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid){
  return flow_stg_spiker(t, (const struct statedata_stg_spiker*)y, (struct statedata_stg_spiker*)dydt, inputs, outputs, first_iteration, modelid);
}

__DEVICE__ unsigned int Stop;

__GLOBAL__ void clearStop(void) {
  Stop = 0;
}

#if defined TARGET_GPU
__GLOBAL__ void exec_kernel_gpu(INTEGRATION_MEM *mem){
  const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;
  
  unsigned int num_iterations;
  
  if (modelid < NUM_MODELS) {
  
  init_output_buffer((output_buffer*)(mem->props->ob), modelid);
  
  for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){
    // Check if simulation is complete
    if(((output_buffer*)(mem->props->ob))->finished[modelid]){
      break;
    }
    if(mem->props->time[modelid] >= mem->props->stoptime){
      ((output_buffer*)(mem->props->ob))->finished[modelid] = 1;
      atomicDec(&((output_buffer*)(mem->props->ob))->active_models, mem->props->num_models);
      break;
    }

    // Check if buffer is full - may not be needed
    atomicOr(&Stop, ((output_buffer*)(mem->props->ob))->full[modelid]);
    // Break if any buffer is full
    if(Stop) { break; }
    
    SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);
    buffer_outputs(mem->props->time[modelid], mem->props->starttime, mem->props->stoptime, (output_data*)mem->props->outputs, (output_buffer*)mem->props->ob, modelid);
    
  }
  
  }
}
#endif

#if defined TARGET_OPENMP
void exec_kernel_openmp(INTEGRATION_MEM *mem, unsigned int modelid){
  unsigned int num_iterations;
  
  init_output_buffer((output_buffer*)(mem->props->ob), modelid);
  
  for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){
    // Check if simulation is complete
    if(((output_buffer*)(mem->props->ob))->finished[modelid]){
      break;
    }
    if(mem->props->time[modelid] >= mem->props->stoptime){
      ((output_buffer*)(mem->props->ob))->finished[modelid] = 1;
      #pragma omp critical
        --((output_buffer*)(mem->props->ob))->active_models;
      break;
    }
    
    SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);
    buffer_outputs(mem->props->time[modelid], mem->props->stoptime, ((output_data*)mem->props->outputs), ((output_buffer*)mem->props->ob), modelid);
    
    if (((output_buffer *)(mem->props->ob))->full[modelid])
      { break; }
  }
}
#endif
#if defined TARGET_CPU
void exec_kernel_cpu(INTEGRATION_MEM *mem) {
  init_output_buffer((output_buffer*)(mem->props->ob), 0);
  
  while (mem->props->time[0] < mem->props->stoptime) {
    SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, 0);
    if (((output_buffer *)(mem->props->ob))->full[0])
      { break; }
  }
  if (mem->props->time[0] >= mem->props->stoptime) {
    { --((output_buffer*)(mem->props->ob))->active_models; }
  }
}
#endif

__GLOBAL__ void registerVBO(float4 *vbo, float4 *cbo) {
  VB = vbo;
  CB = cbo;
}

EXTERN_C
void simengine_register_vertex_buffer(float4 *vb, float4 *cb) {
  registerVBO<<<1,1>>>(vb, cb);
}

__GLOBAL__ void registerCLUT(float4 *clut, unsigned int length) {
  CLUT = clut;
  CLUT_LENGTH = length;
}

EXTERN_C
void simengine_register_clut(float4 *clut, unsigned int length) {
  if (dclut) {
    cutilSafeCall(cudaFree(dclut));
  }
  cutilSafeCall(cudaMalloc((void**)&dclut, length * sizeof(float4)));
  cutilSafeCall(cudaMemcpy(dclut, clut, length * sizeof(float4), cudaMemcpyHostToDevice));
  registerCLUT<<<1,1>>>(dclut, length);
}

EXTERN_C
INTEGRATION_MEM *simengine_init_solver(solver_props **props, CDATAFORMAT *t, CDATAFORMAT t1, CDATAFORMAT *inputs, CDATAFORMAT *model_states) {
  unsigned int modelid = 0;
  solver_props *p;
  if (!(p = (solver_props *)se_alloc.malloc(sizeof(solver_props)))) {
    return 0;
  }

  p->timestep = DT;
  p->abstol = ABSTOL;
  p->reltol = RELTOL;
  p->starttime = *t;
  p->stoptime = t1;
  p->time = t;
  p->model_states = model_states;
  p->inputs = inputs;
  p->outputs = (CDATAFORMAT *)OD;
  p->first_iteration = TRUE;
  p->statesize = seint.num_states;
  p->inputsize = seint.num_inputs;
  p->outputsize = sizeof(output_data) / sizeof(CDATAFORMAT);
  p->num_models = semeta.num_models;
  p->ob_size = sizeof(output_buffer);
  p->ob =& OB;

  *props = p;

  for(modelid=0;modelid<semeta.num_models;modelid++){
    OB.finished[modelid] = 0;
  }
  OB.active_models = NUM_MODELS;

  return SOLVER(INTEGRATION_METHOD, init, TARGET, SIMENGINE_STORAGE, *props);
}

EXTERN_C
void simengine_free_solver(INTEGRATION_MEM *mem, solver_props *props) {
  se_alloc.free(props);
  SOLVER(INTEGRATION_METHOD, free, TARGET, SIMENGINE_STORAGE, mem);
}

EXTERN_C
int simengine_invoke_kernel(INTEGRATION_MEM *mem, solver_props *props, simengine_output *outputs) {
  #if defined TARGET_OPENMP
  omp_set_num_threads(NUM_MODELS);
  #pragma omp parallel
  {
    unsigned int mid = omp_get_thread_num();
    exec_kernel_openmp(mem, mid);
    /*
    if(0 != log_outputs(&OB, outputs, mid))
      { status = ERRMEM; }
    */
  }
  if(status != SUCCESS) break;
  #elif defined TARGET_GPU
  unsigned int nthreads, nblocks, modelid;
  nthreads = BLOCK_SIZE < NUM_MODELS ? BLOCK_SIZE : NUM_MODELS;
  nblocks = (NUM_MODELS + BLOCK_SIZE - 1) / BLOCK_SIZE;
  
  clearStop<<<1,1>>>();
  exec_kernel_gpu<<<nblocks, nthreads>>>(mem);
  cutilSafeCall(cudaMemcpy(&OB, props->ob, props->ob_size, cudaMemcpyDeviceToHost));
  /*
  for (modelid = 0; modelid < semeta.num_models; ++modelid) {
    if (SUCCESS != log_outputs(&OB, outputs, modelid))
      { return ERRMEM; }
  }
  */
  #else
  exec_kernel_cpu(mem);
  /*
  if(SUCCESS != log_outputs(&OB, outputs, 0))
    { return ERRMEM; }
  */
  #endif

  return SUCCESS;
}

EXTERN_C
int simengine_async_invoke_kernel(INTEGRATION_MEM *mem, solver_props *props) {
  unsigned int nthreads, nblocks, modelid;
  nthreads = BLOCK_SIZE < NUM_MODELS ? BLOCK_SIZE : NUM_MODELS;
  nblocks = (NUM_MODELS + BLOCK_SIZE - 1) / BLOCK_SIZE;
  
  clearStop<<<1,1>>>();
  exec_kernel_gpu<<<nblocks, nthreads>>>(mem);
  return SUCCESS;
}

EXTERN_C
int simengine_sync_kernel(solver_props *props, simengine_output *outputs) {
  cutilSafeCall(cudaMemcpy(&OB, props->ob, props->ob_size, cudaMemcpyDeviceToHost));
  /*
  for (modelid = 0; modelid < semeta.num_models; ++modelid) {
    if (SUCCESS != log_outputs(&OB, outputs, modelid))
      { return ERRMEM; }
  }
  */
  return SUCCESS;
  }

int exec_loop(simengine_output *outputs, solver_props *props, INTEGRATION_MEM *mem) {
  int status;
  
  while (OB.active_models) {
    status = simengine_invoke_kernel(mem, props, outputs);
    if (SUCCESS != status) {
      return status;
    }
  }
  
  return SUCCESS;
}


/* Transmutes the internal data buffer into the structured output
 * which may be retured to the client.
 */
/*
int log_outputs(output_buffer *ob, simengine_output *outputs, unsigned int modelid) {
  unsigned int outputid, nquantities, dataid, quantityid;
  simengine_output *output;
  double *odata;
  
  unsigned int ndata = ob->count[modelid];
  void *data = &(ob->buffer[modelid * OUTPUT_BUFFER_LENGTH]);
  
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
*/

EXTERN_C
const simengine_interface *simengine_getinterface(void){
  return &seint;
}

EXTERN_C
const void *simengine_getoutputs(void) {
  return &OB;
}

EXTERN_C
void simengine_init_alloc(simengine_alloc *alloc) {
  if(alloc){
    se_alloc.malloc = alloc->malloc;
    se_alloc.realloc = alloc->realloc;
    se_alloc.free = alloc->free;
  }
}

EXTERN_C
simengine_result *simengine_init_result(unsigned int num_models) {
  unsigned int modelid, outputid;
  simengine_result *seresult;

  if (!(seresult = (simengine_result*)se_alloc.malloc(sizeof(simengine_result)))) {
    return 0;
  }

  // Checks that the number of models matches
  if(num_models != semeta.num_models){
    seresult->status = ERRNUMMDL;
    seresult->status_message = simengine_errors[seresult->status];
    seresult->outputs = 0;
    seresult->final_time = 0;
    return seresult;
  }

  // Allocates return structures
  seresult->outputs = (simengine_output*)se_alloc.malloc(semeta.num_models * seint.num_outputs * sizeof(simengine_output));
  if(!seresult->outputs){
    seresult->status = ERRMEM;
    seresult->status_message = simengine_errors[seresult->status];
    seresult->final_time = 0;
    return seresult;
  }

  seresult->final_time = (double*)se_alloc.malloc(semeta.num_models * sizeof(double));
  if(!seresult->final_time){
    seresult->status = ERRMEM;
    seresult->status_message = simengine_errors[seresult->status];
    seresult->final_time = 0;
    return seresult;
  }


  // Initializes the output structures
  for (modelid = 0; modelid < semeta.num_models; ++modelid) {
    for (outputid = 0; outputid < seint.num_outputs; ++outputid) {
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].alloc = START_SIZE;
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].num_quantities = seint.output_num_quantities[outputid];
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].num_samples = 0;
      seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].data = (double*)se_alloc.malloc(START_SIZE*seint.output_num_quantities[outputid]*sizeof(double));
      if (!seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].data) {
	seresult->status = ERRMEM;
	seresult->status_message = simengine_errors[seresult->status];
	return seresult;
      }
    }
  }

  seresult->status = SUCCESS;
  seresult->status_message = simengine_errors[seresult->status];

  return seresult;
}

EXTERN_C
void simengine_release_result(simengine_result *seresult) {
  se_alloc.free(seresult);
}

EXTERN_C
void simengine_init_time_parameters_and_states(double start_time, CDATAFORMAT *t, double *inputs, CDATAFORMAT *parameters, double *states, CDATAFORMAT *model_states) {
  unsigned int modelid, i;

  for (modelid = 0; modelid < semeta.num_models; ++modelid) {
    t[modelid] = start_time;

    for (i = 0; i < MAX(seint.num_inputs, seint.num_states); ++i) {
      if (i < seint.num_inputs) {
	parameters[TARGET_IDX(seint.num_inputs, semeta.num_models, i, modelid)] = inputs[AS_IDX(seint.num_inputs, semeta.num_models, i, modelid)];
      }
      if (i < seint.num_states) {
	model_states[TARGET_IDX(seint.num_states, semeta.num_models, i, modelid)] = states[AS_IDX(seint.num_states, semeta.num_models, i, modelid)];
      }
    }
  }
}

EXTERN_C
void simengine_return_states(double *states, CDATAFORMAT *model_states) {
  unsigned int modelid, stateid;

  for(modelid=0; modelid<semeta.num_models; modelid++){
    for(stateid=0;stateid<seint.num_states;stateid++){
      states[AS_IDX(seint.num_states, semeta.num_models, stateid, modelid)] = model_states[TARGET_IDX(seint.num_states, semeta.num_models, stateid, modelid)];
    }
  }
}

EXTERN_C
simengine_result *simengine_init(unsigned int num_models, double start_time, CDATAFORMAT *t, CDATAFORMAT t1, double *inputs, CDATAFORMAT *parameters, double *states, CDATAFORMAT *model_states, simengine_alloc *alloc, solver_props **props, INTEGRATION_MEM **mem) {
  simengine_init_alloc(alloc);
  
  // Creates result structure
  simengine_result *seresult = simengine_init_result(num_models);
  if (!(seresult && SUCCESS == seresult->status)) {
    return seresult;
  }

  simengine_init_time_parameters_and_states(start_time, t, inputs, parameters, states, model_states);

  *mem = simengine_init_solver(props, t, t1, parameters, model_states);

  return seresult;
}

EXTERN_C
simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states, simengine_alloc *alloc){
  CDATAFORMAT model_states[NUM_MODELS * NUM_STATES];
  CDATAFORMAT parameters[NUM_MODELS * NUM_INPUTS];
  CDATAFORMAT t[NUM_MODELS];
  CDATAFORMAT t1 = stop_time;
  solver_props *props;
  INTEGRATION_MEM *mem;

  simengine_result *seresult = simengine_init(num_models, start_time, t, t1, inputs, parameters, states, model_states, alloc, &props, &mem);
  
  // Run the model
  seresult->status = exec_loop(seresult->outputs, props, mem);
  seresult->status_message = simengine_errors[seresult->status];
  
  simengine_free_solver(mem, props);

  // Copies state values back to state initial value structure
  simengine_return_states(states, model_states);
  
  return seresult;
}
