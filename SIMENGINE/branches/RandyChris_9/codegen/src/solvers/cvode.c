
#include <cvode/cvode.h>
#include <nvector/nvector_serial.h>
#include <cvode/cvode_dense.h>
#include <cvode/cvode_diag.h>
#include <cvode/cvode_band.h>

#if defined (TARGET_GPU) || defined (TARGET_EMUGPU)
#error CVODE not supported on the GPU
#endif

int user_fun_wrapper(CDATAFORMAT t, N_Vector y, N_Vector ydot, void *userdata){
  cvode_mem *mem = (cvode_mem*)userdata;

  model_flows(t,
	      NV_DATA_S(y), // 'y' has already been partitioned on a per model basis
	      NV_DATA_S(ydot), // 'ydot' is storage created by CVODE for the return value
	      &(mem->props->inputs[mem->modelid*mem->props->inputsize]),
	      &(mem->props->outputs[mem->modelid*mem->props->outputsize]),
	      mem->first_iteration,
	      0 // 0 is passed to modelid to prevent flow from indexing model_states, 
	         // which is already indexed by having a separate mem structure per model
	      );

  mem->first_iteration = FALSE;

  return CV_SUCCESS;
}

cvode_mem *SOLVER(cvode, init, TARGET, SIMENGINE_STORAGE, solver_props *props){
  cvode_mem *mem = (cvode_mem*) malloc(props->num_models*sizeof(cvode_mem));
  unsigned int modelid;

  // Only need to create this buffer in the first memory space as we are using this only for scratch
  // and outside of the CVODE solver to compute the last outputs
  mem[0].k1 = (CDATAFORMAT*)malloc(props->statesize*props->num_models*sizeof(CDATAFORMAT));

  for(modelid=0; modelid<props->num_models; modelid++){
    // Set the modelid on a per memory structure basis
    mem[modelid].modelid = modelid;
    // Store solver properties
    mem[modelid].props = props;
    // Create intial value vector
    // This is done to avoid having the change the internal indexing within the flows and for the output_buffer
    mem[modelid].y0 = N_VMake_Serial(props->statesize, &(props->model_states[modelid*props->statesize]));
    // Create data structure for solver
    //    mem[modelid].cvmem = CVodeCreate(CV_BDF, CV_NEWTON);
    mem[modelid].cvmem = CVodeCreate(props->cvode.lmm, props->cvode.iter);
    
    // Initialize CVODE
    if(CVodeInit(mem[modelid].cvmem, user_fun_wrapper, props->starttime, ((N_Vector)(mem[modelid].y0))) != CV_SUCCESS){
      fprintf(stderr, "Couldn't initialize CVODE");
    }
    // Set solver tolerances
    if(CVodeSStolerances(mem[modelid].cvmem, props->reltol, props->abstol) != CV_SUCCESS){
      fprintf(stderr, "Could not set CVODE tolerances");
    }
    // Set linear solver
    switch (mem[modelid].props->cvode.solv) {
    case CVODE_DENSE:
      if(CVDense(mem[modelid].cvmem, mem[modelid].props->statesize) != CV_SUCCESS){
	fprintf(stderr, "Could not set CVODE DENSE linear solver");
      }
      break;
    case CVODE_DIAG:
      if(CVDiag(mem[modelid].cvmem) != CV_SUCCESS){
	fprintf(stderr, "Could not set CVODE DIAG linear solver");
      }
      break;
    case CVODE_BAND:
      if(CVBand(mem[modelid].cvmem, mem[modelid].props->statesize, ((int*)mem[modelid].props->cvode.solv_opts)[0], ((int*)mem[modelid].props->cvode.solv_opts)[1]) != CV_SUCCESS){
	fprintf(stderr, "Could not set CVODE BAND linear solver");
      }
      break;
    default:
      fprintf(stderr, "No valid CVODE solver passed");
      }

    // Set user data to contain pointer to memory structure for use in model_flows
    if(CVodeSetUserData(mem[modelid].cvmem, &mem[modelid]) != CV_SUCCESS){
      fprintf(stderr, "CVODE failed to initialize user data");
    }
  }

  return mem;
}

int SOLVER(cvode, eval, TARGET, SIMENGINE_STORAGE, cvode_mem *mem, unsigned int modelid) {
  // Stop the solver if the stop time has been reached
  mem->props->running[modelid] = mem->props->time[modelid] < mem->props->stoptime;
  if(!mem->props->running[modelid])
    return 0;

  mem[modelid].first_iteration = TRUE;
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
  free(mem[0].k1);
  free(mem);
}
