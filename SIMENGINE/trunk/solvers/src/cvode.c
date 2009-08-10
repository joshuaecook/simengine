
#include "solvers.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cvode/cvode.h>
#include <nvector/nvector_serial.h>
#include <cvode/cvode_dense.h>

int user_fun_wrapper(CDATAFORMAT t, N_Vector y, N_Vector ydot, void *userdata){
  cvode_mem *mem = userdata;

  (mem->props->fun)(t,
		   NV_DATA_S(y), 
		   NV_DATA_S(ydot),
		   mem->props->inputs,
		   mem->props->outputs,
		   mem->props->first_iteration);

  return CV_SUCCESS;
}

cvode_mem * cvode_init(solver_props *props){
  cvode_mem *mem = (cvode_mem*) malloc(sizeof(cvode_mem));

  N_Vector temp;

  mem->props = props;

  // Create intial value vector
  temp = N_VMake_Serial(mem->props->statesize, mem->props->model_states);
  //temp = N_VNew_Serial(mem->props->statesize);
  mem->y0 = temp;
  // Create data structure for solver
  mem->cvmem = CVodeCreate(CV_BDF, CV_NEWTON);
  // Initialize CVODE
  if(CVodeInit(mem->cvmem, user_fun_wrapper, mem->props->starttime, ((N_Vector)(mem->y0))) != CV_SUCCESS){
    fprintf(stderr, "Couldn't initialize CVODE");
  }
  // Set solver tolerances
  if(CVodeSStolerances(mem->cvmem, props->reltol, props->abstol) != CV_SUCCESS){
    fprintf(stderr, "Could not set CVODE tolerances");
  }
  // Set linear solver
  if(CVDense(mem->cvmem, mem->props->statesize) != CV_SUCCESS){
    fprintf(stderr, "Could not set CVODE linear solver");
  }

  if(CVodeSetUserData(mem->cvmem, mem) != CV_SUCCESS){
    fprintf(stderr, "CVODE failed to initialize user data");
  }

  return mem;
}

int cvode_eval(cvode_mem *mem) {

  if(CVode(mem->cvmem, mem->props->stoptime, ((N_Vector)(mem->y0)), mem->props->time, CV_ONE_STEP) != CV_SUCCESS){
    fprintf(stderr, "CVODE failed to make a step");
    return 1;
  }

  return 0;
}

void cvode_free(cvode_mem *mem){

  /* // Debug code
  long int count;
  if (CVodeGetNumSteps(mem->cvmem, &count) == CV_SUCCESS)
    fprintf(stderr, "Total Number of steps: %ld\n", count);
  if (CVodeGetNumRhsEvals(mem->cvmem, &count) == CV_SUCCESS)
    fprintf(stderr, "RHS Evals: %ld\n", count);
  if (CVodeGetNumErrTestFails(mem->cvmem, &count) == CV_SUCCESS)
    fprintf(stderr, "Num of step errors: %ld\n", count);
  */

  // Cleanup
  N_VDestroy_Serial(((N_Vector)(mem->y0)));
  CVodeFree(&(mem->cvmem));
  free(mem);
}
