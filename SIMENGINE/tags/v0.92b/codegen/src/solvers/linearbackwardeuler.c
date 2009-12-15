
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
void bandsolv_dense(const int n, const int hbw, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid);
__DEVICE__
void bandsolv_sparse(const int n, const int hbw, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid);

__HOST__
int linearbackwardeuler_init(solver_props *props){
  solver_mem *mem;

#if defined TARGET_GPU
  // Allocates GPU global memory for solver's persistent data
  cutilSafeCall(cudaMalloc((void **)&mem, props->num_models * props->statesize * props->bandsize * sizeof(CDATAFORMAT)));
#else // CPU and OPENMP targets
  mem = (solver_mem *)malloc(props->num_models * props->statesize * props->bandsize * sizeof(CDATAFORMAT));
#endif

  props->mem = mem; /* The matrix */

  return 0;
}

__DEVICE__
int linearbackwardeuler_eval(solver_props *props, unsigned int modelid){
  CDATAFORMAT* M = (CDATAFORMAT *)props->mem;

  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  props->running[modelid] = props->time[modelid] + props->timestep <= props->stoptime;
  if(!props->running[modelid])
    return 0;

  int ret = model_flows(props->time[modelid], props->model_states, props->next_states/*b_x*/, props, 1, modelid);

  bandsolv_sparse(props->statesize, props->bandsize, M, props->next_states, props->num_models, modelid);

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

/* Solves a banded linear system of Mx = b for x given M and b
 * Assumes that the matrix M is banded and sorted with all bands along the
 * diagonal.  All memory should be preallocated, including the return value x.
 *
 * M is an n*n matrix with hbw*2+1 bands (bands must be centered around the diagonal.)
 *
 * b_x is the input vector b and reused as the output vector x, both of length n
 */

__DEVICE__
void bandsolv_dense(const int n, const int bw, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid){
  CDATAFORMAT rmult; /* Constant multiplier for a row when eliminating a value from another row */
  int br, er, bc, ec; /* Row and column matrix indicies */
  int idx; /* Vector index */
  int hbw = bw>>1; // Integer division by 2

  /* Clear out lower half of M */
  for(br = bc = 0; br < n-1; br++, bc++){
    for(er = br + 1; er <= MIN(br + hbw, n-1); er++){
      rmult = -M[MATIDX(n,n,er,bc, num_models, modelid)]/M[MATIDX(n,n,br,bc, num_models, modelid)];
      for(ec = bc + 1; ec <= MIN(bc + hbw, n); ec++){
	M[MATIDX(n,n,er,ec, num_models, modelid)] += rmult*M[MATIDX(n,n,br,ec, num_models, modelid)];
      }
      b_x[VECIDX(n,er,num_models,modelid)] += rmult*b_x[VECIDX(n,br,num_models,modelid)];
    }
  }

  /* Clear out upper half of M */
  for(br = bc = n-1; br > 0; br--, bc--){
    for(er = br - 1; er >= MAX(br - hbw, 0); er--){
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
void bandsolv_sparse(const int n, const int bw, CDATAFORMAT *M, CDATAFORMAT *b_x, unsigned int num_models, unsigned int modelid){
  CDATAFORMAT rmult; /* Constant multiplier for a row when eliminating a value from another row */
  int br, er, bc, ec; /* Row and column matrix indicies */
  int hbw = bw>>1; // Integer division by 2

  /* Clear out lower half of M */
  for(br = 0; br < n-1; br++){
    for(er = br + 1; er <= MIN(br + hbw, n-1); er++){
      bc = hbw;
      ec = bc - (er-br);
      rmult = -M[MATIDX(n,bw,er,ec, num_models, modelid)]/M[MATIDX(n,bw,br,bc, num_models, modelid)];
      for(; bc < 2*hbw+1; ec++, bc++){
	M[MATIDX(n,bw,er,ec, num_models, modelid)] += rmult*M[MATIDX(n,bw,br,bc, num_models, modelid)];
      }
      b_x[VECIDX(n,er,num_models,modelid)] += rmult*b_x[VECIDX(n,br,num_models,modelid)];
    }
  }

  /* Clear out upper half of M */
  bc = hbw;
  for(br = n-1; br > 0; br--){
    for(er = br - 1; er >= MAX(br - hbw, 0); er--){
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
