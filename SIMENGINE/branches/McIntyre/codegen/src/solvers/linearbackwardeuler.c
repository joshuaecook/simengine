#include <solvers.h>

#define MATIDX(n,r,c) (r*n+c) /* Row major ordering */

#if NUM_MODELS > 1
#error Linear solver not parallelized to multiple models
#end

typedef struct{
  CDATAFORMAT *M; // sparse nxn banded matrix
} bandsolv_mem;

void bandsolv_dense(const int n, const int hbw, CDATAFORMAT *M, CDATAFORMAT *b_x);
void bandsolv_sparse(const int n, const int hbw, CDATAFORMAT *M, CDATAFORMAT *b_x);

int linearbackwardeuler_init(solver_props *props){
  bandsolv_mem *mem = (bandsolv_mem*) malloc(sizeof(bandsolv_mem));

  props->mem = mem;
  mem->M = (CDATAFORMAT*)malloc(props->statesize*props->bandsize*sizeof(CDATAFORMAT));

  return 0;
}

int linearbackwardeuler_eval(solver_props *props, unsigned int modelid){
  bandsolv_mem *mem = props->mem;
  int hbw = props->bandsize>>1; // Integer division by 2

  // Stop the simulation if the next step will be beyond the stoptime (this will hit the stoptime exactly for even multiples unless there is rounding error)
  props->running[modelid] = props->time[modelid] + props->timestep <= props->stoptime;
  if(!props->running[modelid])
    return 0;

  int ret = model_flows_linear(props->model_states, mem->M, props->next_states/*b_x*/, modelid);

  bandsolv_sparse(props->statesize, hbw, 
		  mem->M + (modelid*props->statesize*props->bandsize),
		  props->next_states + (modelid*props->statesize));

  props->nexttime[modelid] += props->timestep;

  return ret;
}

int linearbackwardeuler_free(solver_props *props){
  bandsolv_mem *mem = props->mem;
  free(mem->M);
  free(mem);
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

void bandsolv_dense(const int n, const int hbw, CDATAFORMAT *M, CDATAFORMAT *b_x){
  CDATAFORMAT rmult; /* Constant multiplier for a row when eliminating a value from another row */
  int br, er, bc, ec; /* Row and column matrix indicies */
  int idx; /* Vector index */

  /* Clear out lower half of M */
  for(br = bc = 0; br < n-1; br++, bc++){
    for(er = br + 1; er <= MIN(br + hbw, n); er++){
      rmult = -M[IDX(er,bc)]/M[IDX(br,bc)];
      for(ec = bc + 1; ec <= MIN(bc + hbw, n); ec++){
	M[IDX(er,ec)] += rmult*M[IDX(br,ec)];
      }
      b_x[er] += rmult*b_x[br];
    }
  }

  /* Clear out upper half of M */
  for(br = bc = n-1; br > 0; br--, bc--){
    for(er = br - 1; er >= MAX(br - hbw, 0); er--){
      rmult = -M[IDX(er,bc)]/M[IDX(br,bc)];
      b_x[er] += rmult*b_x[br];
    }
  }

  /* Produce x from remaining diagonal of M */
  for(idx = 0; idx < n; idx++){
    b_x[idx] = b_x[idx]/M[IDX(idx,idx)];
  }
}

void bandsolv_sparse(const int n, const int hbw, CDATAFORMAT *M, CDATAFORMAT *b_x){
  CDATAFORMAT rmult; /* Constant multiplier for a row when eliminating a value from another row */
  int br, er, bc, ec; /* Row and column matrix indicies */

  /* Clear out lower half of M */
  for(br = 0; br < n-1; br++){
    for(er = br + 1; er <= MIN(br + hbw, n); er++){
      bc = hbw;
      ec = bc - (er-br);
      rmult = -M[IDX(er,ec)]/M[IDX(br,bc)];
      for(; bc < 2*hbw+1; ec++, bc++){
	M[IDX(er,ec)] += rmult*M[IDX(br,bc)];
      }
      b_x[er] += rmult*b_x[br];
    }
  }

  /* Clear out upper half of M */
  bc = hbw;
  for(br = n-1; br > 0; br--){
    for(er = br - 1; er >= MAX(br - hbw, 0); er--){
      ec = bc + (br-er);
      rmult = -M[IDX(er,ec)]/M[IDX(br,bc)];
      b_x[er] += rmult*b_x[br];
    }
  }

  /* Produce x from remaining diagonal of M */
  for(br = 0; br < n; br++){
    b_x[br] = b_x[br]/M[IDX(br,bc)];
  }
}
