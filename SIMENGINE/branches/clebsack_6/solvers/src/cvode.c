
#include "solvers.h"
#include <cvode/cvode.h>
#include <nvector/nvector_serial.h>
#include <cvode/cvode_dense.h>

#if defined (TARGET_GPU) || defined (TARGET_EMUGPU)
#error CVODE not supported on the GPU
#endif

int user_fun_wrapper(CDATAFORMAT t, N_Vector y, N_Vector ydot, void *userdata){
  cvode_mem *mem = userdata;

  model_flows(t,
	      NV_DATA_S(y), 
	      NV_DATA_S(ydot),
	      mem->props->inputs,
	      mem->props->outputs,
	      mem->props->ob,
	      mem->props->first_iteration,
	      mem->modelid);

  return CV_SUCCESS;
}

cvode_mem *SOLVER(cvode, init, TARGET, SIMENGINE_STORAGE, solver_props *props){
  cvode_mem *mem = (cvode_mem*) malloc(props->num_models*sizeof(cvode_mem));
  unsigned int modelid;

  for(modelid=0; modelid<props->num_models; modelid++){
    // Set the modelid on a per memory structure basis
    mem[modelid].modelid = modelid;
    // Store solver properties
    mem[modelid].props = props;
    // Create intial value vector
    // This is overkill, creating a copy of all states for all models for every single model
    // This is done to avoid having the change the internal indexing within the flows and for the output_buffer
    mem[modelid].y0 = N_VMake_Serial(props->num_models*props->statesize, props->model_states);
    // Create data structure for solver
    mem[modelid].cvmem = CVodeCreate(CV_BDF, CV_NEWTON);
    // Initialize CVODE
    if(CVodeInit(mem[modelid].cvmem, user_fun_wrapper, 0, ((N_Vector)(mem[modelid].y0))) != CV_SUCCESS){
      fprintf(stderr, "Couldn't initialize CVODE");
    }
    // Set solver tolerances
    if(CVodeSStolerances(mem[modelid].cvmem, props->reltol, props->abstol) != CV_SUCCESS){
      fprintf(stderr, "Could not set CVODE tolerances");
    }
    // Set linear solver
    if(CVDense(mem[modelid].cvmem, mem[modelid].props->statesize) != CV_SUCCESS){
      fprintf(stderr, "Could not set CVODE linear solver");
    }
    // Set user data to contain pointer to memory structure for use in model_flows
    if(CVodeSetUserData(mem[modelid].cvmem, &mem[modelid]) != CV_SUCCESS){
      fprintf(stderr, "CVODE failed to initialize user data");
    }
  }

  return mem;
}

int SOLVER(cvode, eval, TARGET, SIMENGINE_STORAGE, cvode_mem *mem, unsigned int modelid) {
  if(CVode(mem[modelid].cvmem, mem[modelid].props->stoptime, ((N_Vector)(mem[modelid].y0)), &(mem[modelid].props->time[modelid]), CV_ONE_STEP) != CV_SUCCESS){
    fprintf(stderr, "CVODE failed to make a step in model %d.\n", modelid);
    return 1;
  }

  return 0;
}

void SOLVER(cvode, free, TARGET, SIMENGINE_STORAGE, cvode_mem *mem){
  unsigned int modelid;
  /* // Debug code
  long int count;
  for(modelid=0; modelid<mem[0].props->num_models; modelid++){
    fprintf(stderr, "Model number: %d\n", modelid);
    if (CVodeGetNumSteps(mem->cvmem, &count) == CV_SUCCESS)
      fprintf(stderr, "  Total Number of steps: %ld\n", count);
    if (CVodeGetNumRhsEvals(mem->cvmem, &count) == CV_SUCCESS)
      fprintf(stderr, "  RHS Evals: %ld\n", count);
    if (CVodeGetNumErrTestFails(mem->cvmem, &count) == CV_SUCCESS)
      fprintf(stderr, "  Num of step errors: %ld\n", count);
  }
  */

  // Cleanup
  for(modelid = 0; modelid<mem[0].props->num_models; modelid++){
    N_VDestroy_Serial(((N_Vector)(mem[modelid].y0)));
    CVodeFree(&(mem[modelid].cvmem));
  }
  free(mem);
}
