
enum { LSOLVER_DENSE,
       LSOLVER_BANDED
};

typedef struct{
  unsigned int lsolver;
  unsigned int upperhalfbw;  // Number of upper bands in statesize*statesize matrix for linear solver
  unsigned int lowerhalfbw;  // Number of lower bands in statesize*statesize matrix for linear solver
}linearbackwardeuler_opts;


__DEVICE__
void lsolver_dense(const int n, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid);
__DEVICE__
void lsolver_banded(const int n, const int lbw, const int ubw, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid);

__HOST__
int linearbackwardeuler_init(solver_props *props){
  solver_mem *mem;
  linearbackwardeuler_opts *opts = (linearbackwardeuler_opts*)&props->opts;
  unsigned int bandwidth = opts->upperhalfbw + opts->lowerhalfbw + 1;

#if defined TARGET_GPU
  // Allocates GPU global memory for solver's persistent data
  switch(opts->lsolver){
  case LSOLVER_DENSE:
    cutilSafeCall(cudaMalloc((void **)&mem, PARALLEL_MODELS * props->statesize * props->statesize * sizeof(CDATAFORMAT)));
    break;
  case LSOLVER_BANDED:
    cutilSafeCall(cudaMalloc((void **)&mem, PARALLEL_MODELS * props->statesize * bandwidth * sizeof(CDATAFORMAT)));
    break;
  default:
    return 1;
  }
#else // CPU and OPENMP targets
  switch(opts->lsolver){
  case LSOLVER_DENSE:
    mem = (solver_mem *)malloc(PARALLEL_MODELS * props->statesize * props->statesize * sizeof(CDATAFORMAT));
    break;
  case LSOLVER_BANDED:
    mem = (solver_mem *)malloc(PARALLEL_MODELS * props->statesize * bandwidth * sizeof(CDATAFORMAT));
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

  // Produce the matrix M and vector b
  int ret = model_flows(props->time[modelid], props->model_states, props->next_states/*b_x*/, props, 1, modelid);

  // Run the specified linear solver
  switch(opts->lsolver){
  case LSOLVER_DENSE:
    lsolver_dense(props->statesize, M, props->next_states, PARALLEL_MODELS, modelid);
    break;
  case LSOLVER_BANDED:
    lsolver_banded(props->statesize, opts->lowerhalfbw, opts->upperhalfbw, M, props->next_states, PARALLEL_MODELS, modelid);
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
      if(M[MAT_IDX(n,n,er,bc, num_models, modelid)] != 0){
	rmult = -M[MAT_IDX(n,n,er,bc, num_models, modelid)]/M[MAT_IDX(n,n,br,bc, num_models, modelid)];
	for(ec = bc + 1; ec < n; ec++){
	  M[MAT_IDX(n,n,er,ec, num_models, modelid)] += rmult*M[MAT_IDX(n,n,br,ec, num_models, modelid)];
	}
	b_x[VEC_IDX(n,er,num_models,modelid)] += rmult*b_x[VEC_IDX(n,br,num_models,modelid)];
      }
    }
  }

  /* Clear out upper half of M */
  for(br = bc = n-1; br > 0; br--, bc--){
    for(er = br - 1; er >= 0; er--){
      if(M[MAT_IDX(n,n,er,bc, num_models, modelid)] != 0){
	rmult = -M[MAT_IDX(n,n,er,bc, num_models, modelid)]/M[MAT_IDX(n,n,br,bc, num_models, modelid)];
	b_x[VEC_IDX(n,er,num_models,modelid)] += rmult*b_x[VEC_IDX(n,br,num_models,modelid)];
      }
    }
  }

  /* Produce x from remaining diagonal of M */
  for(idx = 0; idx < n; idx++){
    b_x[VEC_IDX(n,idx,num_models,modelid)] = b_x[VEC_IDX(n,idx,num_models,modelid)]/M[MAT_IDX(n,n,idx,idx, num_models, modelid)];
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
      if(M[MAT_IDX(n,bw,er,ec, num_models, modelid)] != 0){
	rmult = -M[MAT_IDX(n,bw,er,ec, num_models, modelid)]/M[MAT_IDX(n,bw,br,bc, num_models, modelid)];
	for(; bc < bw; ec++, bc++){
	  M[MAT_IDX(n,bw,er,ec, num_models, modelid)] += rmult*M[MAT_IDX(n,bw,br,bc, num_models, modelid)];
	}
	b_x[VEC_IDX(n,er,num_models,modelid)] += rmult*b_x[VEC_IDX(n,br,num_models,modelid)];
      }
    }
  }

  /* Clear out upper half of M */
  bc = lbw;
  for(br = n-1; br > 0; br--){
    for(er = br - 1; er >= MAX(br - ubw, 0); er--){
      ec = bc + (br-er);
      if(M[MAT_IDX(n,bw,er,ec, num_models, modelid)] != 0){
	rmult = -M[MAT_IDX(n,bw,er,ec, num_models, modelid)]/M[MAT_IDX(n,bw,br,bc, num_models, modelid)];
	b_x[VEC_IDX(n,er,num_models,modelid)] += rmult*b_x[VEC_IDX(n,br,num_models,modelid)];
      }
    }
  }

  /* Produce x from remaining diagonal of M */
  for(br = 0; br < n; br++){
    b_x[VEC_IDX(n,br,num_models,modelid)] = b_x[VEC_IDX(n,br,num_models,modelid)]/M[MAT_IDX(n,bw,br,bc, num_models, modelid)];
  }
}
