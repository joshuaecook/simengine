#include <string.h>
#include <cvode/cvode.h>
#include <nvector/nvector_serial.h>
#include <cvode/cvode_dense.h>
#include <cvode/cvode_diag.h>
#include <cvode/cvode_band.h>

#if defined (TARGET_GPU) || defined (TARGET_EMUGPU)
#error CVODE not supported on the GPU
#endif

typedef struct{
  solver_props *props;
  CDATAFORMAT *next_states;
  void *cvmem;
  N_Vector y0;
  unsigned int modelid;
  unsigned int first_iteration;
} cvode_mem;

int user_fun_wrapper(CDATAFORMAT t, N_Vector y, N_Vector ydot, void *userdata){
  cvode_mem *mem = (cvode_mem*)userdata;
  solver_props *props = mem->props;
  unsigned int modelid = mem->modelid;

  model_flows(t,
	      NV_DATA_S(y) - (modelid*props->statesize), // Model flows indexes y and dydt with modelid
	      NV_DATA_S(ydot) - (modelid*props->statesize), // and this ptr arithmetic adjusts to compensate
	      props, mem->first_iteration, modelid);

  mem->first_iteration = FALSE;

  return CV_SUCCESS;
}

void cvode_err_handler(int error_code, const char *module, const char *function, char *msg, void *eh_data){
  //cvode_mem *mem = (cvode_mem*)eh_data; // In case we want to know more details?
  if(error_code <= 0){
    PRINTF("%s ERROR %d: %s : %s\n", module, error_code, function, msg);
  }
  else{
    PRINTF("%s WARNING %d: %s : %s\n", module, error_code, function, msg);
  }
}

int cvode_init(solver_props *props){
  assert(props->statesize > 0);

  cvode_mem *mem = (cvode_mem*) malloc(props->num_models*sizeof(cvode_mem));
  unsigned int modelid;

  props->mem = mem;

  for(modelid=0; modelid<props->num_models; modelid++){
    // Set location to store the value of the next states
    mem[modelid].next_states = &(props->next_states[modelid*props->statesize]);
    mem[modelid].props = props;
    // Set the modelid on a per memory structure basis
    mem[modelid].modelid = modelid;
    // Create intial value vector
    // This is done to avoid having the change the internal indexing within the flows and for the output_buffer
    mem[modelid].y0 = N_VMake_Serial(props->statesize, mem[modelid].next_states);
    // Create data structure for solver
    //    mem[modelid].cvmem = CVodeCreate(CV_BDF, CV_NEWTON);
    mem[modelid].cvmem = CVodeCreate(props->cvode.lmm, props->cvode.iter);
    
    // Initialize CVODE
    if(CVodeInit(mem[modelid].cvmem, user_fun_wrapper, props->starttime, mem[modelid].y0) != CV_SUCCESS){
      PRINTF( "Couldn't initialize CVODE");
    }
    // Set CVODE error handler
    if(CVodeSetErrHandlerFn(mem[modelid].cvmem, cvode_err_handler, mem)){
      PRINTF( "Couldn't set CVODE error handler");
    }
    // Set solver tolerances
    if(CVodeSStolerances(mem[modelid].cvmem, props->reltol, props->abstol) != CV_SUCCESS){
      PRINTF( "Could not set CVODE tolerances");
    }
    // Set maximum order
    if(CVodeSetMaxOrd(mem[modelid].cvmem, props->cvode.max_order) != CV_SUCCESS) {
      PRINTF( "Could not set CVODE maximum order");
    }
    // Set linear solver
    switch (props->cvode.solv) {
    case CVODE_DENSE:
      if(CVDense(mem[modelid].cvmem, mem[modelid].props->statesize) != CV_SUCCESS){
	PRINTF( "Could not set CVODE DENSE linear solver");
      }
      break;
    case CVODE_DIAG:
      if(CVDiag(mem[modelid].cvmem) != CV_SUCCESS){
	PRINTF( "Could not set CVODE DIAG linear solver");
      }
      break;
    case CVODE_BAND:
      if(CVBand(mem[modelid].cvmem, mem[modelid].props->statesize, mem[modelid].props->cvode.upperhalfbw, mem[modelid].props->cvode.lowerhalfbw) != CV_SUCCESS){
	PRINTF( "Could not set CVODE BAND linear solver");
      }
      break;
    default:
      PRINTF( "No valid CVODE solver passed");
      }

    // Set user data to contain pointer to memory structure for use in model_flows
    if(CVodeSetUserData(mem[modelid].cvmem, &mem[modelid]) != CV_SUCCESS){
      PRINTF( "CVODE failed to initialize user data");
    }
  }

  return 0;
}

int cvode_eval(solver_props *props, unsigned int modelid){
  cvode_mem *mem = props->mem;
  mem = &mem[modelid];

  // Stop the solver if the stop time has been reached
  props->running[modelid] = (props->time[modelid] + props->timestep) < props->stoptime;
  if(!props->running[modelid])
    return 0;

  // if a positive dt is specified, then we will have this function return after it reaches the next time point,
  // otherwise, it will just run one iteration and return
  if(props->timestep > 0) {
    // Reinitialize the function at each step
    if(CVodeReInit(mem->cvmem, props->time[modelid], mem->y0) != CV_SUCCESS) {
      PRINTF( "CVODE failed to reinitialize");
    }

    CDATAFORMAT stop_time = MIN(props->time[modelid] + props->timestep, props->stoptime);
    mem->first_iteration = TRUE;
    if(CVode(mem->cvmem, stop_time, mem->y0, &(props->next_time[modelid]), CV_NORMAL) != CV_SUCCESS){
      PRINTF( "CVODE failed to make a fixed step in model %d.\n", modelid);
      return 1;
    }
  }
  else {
    mem->first_iteration = TRUE;
    if(CVode(mem->cvmem, props->stoptime, mem->y0, &(props->next_time[modelid]), CV_ONE_STEP) != CV_SUCCESS){
      PRINTF( "CVODE failed to make a step in model %d.\n", modelid);
      return 1;
    }
  }

  return 0;
}

int cvode_free(solver_props *props){
  unsigned int modelid;
  /* // Debug code
  long int count;
  for(modelid=0; modelid<mem[0].props->num_models; modelid++){
    PRINTF( "Model number: %d\n", modelid);
    if (CVodeGetNumSteps(mem->cvmem, &count) == CV_SUCCESS)
      PRINTF( "  Total Number of steps: %ld\n", count);
    if (CVodeGetNumRhsEvals(mem->cvmem, &count) == CV_SUCCESS)
      PRINTF( "  RHS Evals: %ld\n", count);
    if (CVodeGetNumErrTestFails(mem->cvmem, &count) == CV_SUCCESS)
      PRINTF( "  Num of step errors: %ld\n", count);
  }
  */

  // Cleanup
  cvode_mem *mem = props->mem;
  for(modelid = 0; modelid<props->num_models; modelid++){
    N_VDestroy_Serial(((N_Vector)(mem[modelid].y0)));
    CVodeFree(&(mem[modelid].cvmem));
  }
  free(mem);

  return 0;
}
