
enum { LSOLVER_DENSE,
       LSOLVER_BANDED
};

typedef struct{
  unsigned int lsolver;
  unsigned int upperhalfbw;  // Number of upper bands in statesize*statesize matrix for linear solver
  unsigned int lowerhalfbw;  // Number of lower bands in statesize*statesize matrix for linear solver
}linearbackwardeuler_opts;

// Matrix and Vector accessor functions
__HOST__ __DEVICE__
int MATIDX(int nr, int nc, int r, int c, unsigned int num_models, unsigned int modelid){
  int idx = TARGET_IDX(nc*nr, num_models, r*nc+c, modelid);
  return idx;
}

__HOST__ __DEVICE__
int VECIDX(int nc, int c, unsigned int num_models, unsigned int modelid){
  int idx = TARGET_IDX(nc, num_models, c, modelid);
  return idx;
}

__DEVICE__
void lsolver_dense(const int n, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid);
__DEVICE__
void lsolver_banded(const int n, const int lbw, const int ubw, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid);

__HOST__
int linearbackwardeuler_init(solver_props *props){
  solver_mem *mem;
  linearbackwardeuler_opts *opts = (linearbackwardeuler_opts*)&props->opts;

#if defined TARGET_GPU
  // Allocates GPU global memory for solver's persistent data
  switch(opts->lsolver){
  case LSOLVER_DENSE:
    cutilSafeCall(cudaMalloc((void **)&mem, props->num_models * props->statesize * props->statesize * sizeof(CDATAFORMAT)));
    break;
  case LSOLVER_BANDED:
    cutilSafeCall(cudaMalloc((void **)&mem, props->num_models * props->statesize * opts->bandsize * sizeof(CDATAFORMAT)));
    break;
  default:
    return 1;
  }
#else // CPU and OPENMP targets
  switch(opts->lsolver){
  case LSOLVER_DENSE:
    mem = (solver_mem *)malloc(props->num_models * props->statesize * props->statesize * sizeof(CDATAFORMAT));
    break;
  case LSOLVER_BANDED:
    mem = (solver_mem *)malloc(props->num_models * props->statesize * opts->bandsize * sizeof(CDATAFORMAT));
    break;
  default:
    return 1;
  }
#endif

  props->mem = mem; /* The matrix */

  return 0;
}

__DEVICE__
int linearbackwardeuler_eval(solver_props *props, unsigned int modelid){
  CDATAFORMAT* M = (CDATAFORMAT *)props->mem;
  linearbackwardeuler_opts *opts = (linearbackwardeuler_opts*)&props->opts;

  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  props->running[modelid] = props->time[modelid] + props->timestep <= props->stoptime;
  if(!props->running[modelid])
    return 0;

  // Produce the matrix M and vector b
  int ret = model_flows(props->time[modelid], props->model_states, props->next_states/*b_x*/, props, 1, modelid);

  // Run the specified linear solver
  switch(opts->lsolver){
  case LSOLVER_DENSE:
    lsolver_dense(props->statesize, M, props->next_states, props->num_models, modelid);
    break;
  case LSOLVER_BANDED:
    lsolver_banded(props->statesize, opts->lowerhalfbw, opts->upperhalfbw, M, props->next_states, props->num_models, modelid);
    break;
  default:
    return 1;
  }

  props->next_time[modelid] += props->timestep;

  return ret;
}

__HOST__
int linearbackwardeuler_free(solver_props *props){
#if defined TARGET_GPU
  cutilSafeCall(cudaFree(props->mem));
#else // Used for CPU and OPENMP targets
  free(props->mem);
#endif
  return 0;
}

__DEVICE__
void lsolver_dense(const int n, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid){
  CDATAFORMAT rmult; /* Constant multiplier for a row when eliminating a value from another row */
  int br, er, bc, ec; /* Row and column matrix indicies */
  int idx; /* Vector index */

  /* Clear out lower half of M */
  for(br = bc = 0; br < n-1; br++, bc++){
    for(er = br + 1; er < n; er++){
      rmult = -M[MATIDX(n,n,er,bc, num_models, modelid)]/M[MATIDX(n,n,br,bc, num_models, modelid)];
      for(ec = bc + 1; ec < n; ec++){
	M[MATIDX(n,n,er,ec, num_models, modelid)] += rmult*M[MATIDX(n,n,br,ec, num_models, modelid)];
      }
      b_x[VECIDX(n,er,num_models,modelid)] += rmult*b_x[VECIDX(n,br,num_models,modelid)];
    }
  }

  /* Clear out upper half of M */
  for(br = bc = n-1; br > 0; br--, bc--){
    for(er = br - 1; er >= 0; er--){
      rmult = -M[MATIDX(n,n,er,bc, num_models, modelid)]/M[MATIDX(n,n,br,bc, num_models, modelid)];
      b_x[VECIDX(n,er,num_models,modelid)] += rmult*b_x[VECIDX(n,br,num_models,modelid)];
    }
  }

  /* Produce x from remaining diagonal of M */
  for(idx = 0; idx < n; idx++){
    b_x[VECIDX(n,idx,num_models,modelid)] = b_x[VECIDX(n,idx,num_models,modelid)]/M[MATIDX(n,n,idx,idx, num_models, modelid)];
  }
}

__DEVICE__
void lsolver_banded(const int n, const int lbw, const int ubw, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid){
  CDATAFORMAT rmult; /* Constant multiplier for a row when eliminating a value from another row */
  int br, er, bc, ec; /* Row and column matrix indicies */
  int bw = 1+lbw+ubw; /* Complete number of bands */

  /* Clear out lower half of M */
  for(br = 0; br < n-1; br++){
    for(er = br + 1; er <= MIN(br + lbw, n-1); er++){
      bc = lbw;
      ec = bc - (er-br);
      rmult = -M[MATIDX(n,bw,er,ec, num_models, modelid)]/M[MATIDX(n,bw,br,bc, num_models, modelid)];
      for(; bc < bw; ec++, bc++){
	M[MATIDX(n,bw,er,ec, num_models, modelid)] += rmult*M[MATIDX(n,bw,br,bc, num_models, modelid)];
      }
      b_x[VECIDX(n,er,num_models,modelid)] += rmult*b_x[VECIDX(n,br,num_models,modelid)];
    }
  }

  /* Clear out upper half of M */
  bc = lbw;
  for(br = n-1; br > 0; br--){
    for(er = br - 1; er >= MAX(br - ubw, 0); er--){
      ec = bc + (br-er);
      rmult = -M[MATIDX(n,bw,er,ec, num_models, modelid)]/M[MATIDX(n,bw,br,bc, num_models, modelid)];
      b_x[VECIDX(n,er,num_models,modelid)] += rmult*b_x[VECIDX(n,br,num_models,modelid)];
    }
  }

  /* Produce x from remaining diagonal of M */
  for(br = 0; br < n; br++){
    b_x[VECIDX(n,br,num_models,modelid)] = b_x[VECIDX(n,br,num_models,modelid)]/M[MATIDX(n,bw,br,bc, num_models, modelid)];
  }
}
