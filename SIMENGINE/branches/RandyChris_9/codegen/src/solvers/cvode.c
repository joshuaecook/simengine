
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
  CDATAFORMAT *next_states; // Used only to produce last output values
  void *cvmem;
  void *y0;
  unsigned int modelid;
  unsigned int first_iteration;
} cvode_mem;

int user_fun_wrapper(CDATAFORMAT t, N_Vector y, N_Vector ydot, void *userdata){
  cvode_mem *mem = (cvode_mem*)userdata;
  solver_props *props = mem->props;

#error CVODE is BROKEN

  model_flows(t,
	      NV_DATA_S(y), // 'y' has already been partitioned on a per model basis
	      NV_DATA_S(ydot), // 'ydot' is storage created by CVODE for the return value
	      &(props->inputs[mem->modelid*props->inputsize]),  // THIS HAS TO BE CHANGED FOR THE NEW MODEL FLOWS INTERFACE!!!!!!
	      &(props->outputs[mem->modelid*props->outputsize]),
	      mem->first_iteration,
	      0 // 0 is passed to modelid to prevent flow from indexing model_states, 
	         // which is already indexed by having a separate mem structure per model
	      );

  mem->first_iteration = FALSE;

  return CV_SUCCESS;
}

int cvode_init(solver_props *props){
  cvode_mem *mem = (cvode_mem*) malloc(props->num_models*sizeof(cvode_mem));
  unsigned int modelid;

  props->mem = mem;
  mem->next_states = (CDATAFORMAT*)malloc(props->statesize*sizeof(CDATAFORMAT));
  // Initialize next states to state initial values
  memcpy(mem->next_states, props->model_states, props->num_models*props->statesize*sizeof(CDATAFORMAT));
  props->next_states = mem[0].next_states;

  for(modelid=0; modelid<props->num_models; modelid++){
    // Set location to store the value of the next states
    mem[modelid].next_states = &(mem->next_states[modelid*props->statesize]);
    mem[modelid].props = props;
    // Set the modelid on a per memory structure basis
    mem[modelid].modelid = modelid;
    // Create intial value vector
    // This is done to avoid having the change the internal indexing within the flows and for the output_buffer
    mem[modelid].y0 = N_VMake_Serial(props->statesize, mem[modelid].next_states));
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

  return 0;
}

int cvode_eval(solver_props *props, unsigned int modelid){
  cvode_mem *mem = props->mem;

  // Copy the next state values to the statespace
  if(props->time[modelid] != props->starttime){
    memcpy(&(props->model_states[modelid*props->statesize]), mem[modelid].next_states, props->statesize*sizeof(CDATAFORMAT));
  }
  // Stop the solver if the stop time has been reached
  props->running[modelid] = props->time[modelid] < props->stoptime;
  if(!props->running[modelid])
    return 0;

  mem[modelid].first_iteration = TRUE;
  if(CVode(mem[modelid].cvmem, mem[modelid].props->stoptime, ((N_Vector)(mem[modelid].y0)), &(mem[modelid].props->next_time[modelid]), CV_ONE_STEP) != CV_SUCCESS){
    fprintf(stderr, "CVODE failed to make a step in model %d.\n", modelid);
    return 1;
  }

  return 0;
}

int cvode_free(solver_props *props){
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
  cvode_mem *mem = props->mem;
  for(modelid = 0; modelid<props->num_models; modelid++){
    N_VDestroy_Serial(((N_Vector)(mem[modelid].y0)));
    CVodeFree(&(mem[modelid].cvmem));
    free(mem[modelid].next_states);
  }
  free(mem);

  return 0;
}
