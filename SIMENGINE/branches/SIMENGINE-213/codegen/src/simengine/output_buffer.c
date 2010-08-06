indexed_output_buffer *global_ixob = NULL;
#if defined TARGET_GPU
__DEVICE__ indexed_output_buffer *gpu_ixob;
#endif


__DEVICE__ void init_output_buffer(output_buffer *ob, unsigned int modelid){
  ob->full[modelid] = 0;
  ob->count[modelid] = 0;
  ob->ptr[modelid] = &ob->buffer[modelid*BUFFER_LEN];
  ob->end[modelid] = &ob->buffer[(modelid+1)*BUFFER_LEN];
}

/* Destructively computes the prefix sum of an integer vector of size length. */
__DEVICE__ void parallel_scan(int *vector, unsigned int threadid, unsigned int length) {
  unsigned int i, j;
  int participate;

  for (i=0; i<ceilf(log2f(length)); i++) {
    participate = threadid >= exp2f(i);
    if (participate) {
      vector[threadid] += vector[threadid-(int)exp2f(i)];
    }
  }
}

indexed_output_buffer *alloc_indexed_output_buffer (unsigned int gridsize, unsigned int blocksize) {
#if NUM_OUTPUTS == 0
  return NULL;
#endif
#if defined TARGET_GPU
  indexed_output_buffer *g_ixob = NULL;
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_ixob, gpu_ixob));
  cutilSafeCall(cudaMalloc((void **)&g_ixob, gridsize*sizeof(indexed_output_buffer)));
  cutilSafeCall(cudaMemset(g_ixob,0,gridsize*sizeof(indexed_output_buffer)));
  return g_ixob;
#else
  unsigned int i;
  global_ixob = (indexed_output_buffer *)calloc(gridsize,sizeof(indexed_output_buffer));
  return global_ixob;
#endif  
}

/* Assigns the scratch pointer for this thread's buffer. 
 * The caller should allocate the scratch memory (blocksize*sizeof(int))
 * but the buffer takes ownership; it is freed by free_indexed_output_buffer().
 */
__DEVICE__ void init_indexed_output_buffer (indexed_output_buffer *buffer, int *scratch, unsigned int threadid) {
  if (0 == threadid) {
    buffer->scratch = scratch;
  }
}

/* Frees the memory allocated for buffers and scratch space. */
void free_indexed_output_buffer (indexed_output_buffer *buffer, unsigned int gridsize) {
#if defined TARGET_GPU
  indexed_output_buffer *g_ixob = NULL;
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_ixob, gpu_ixob));
  cutilSafeCall(cudaFree(g_ixob));
#else
  unsigned int i;
  for (i=0; i<gridsize; i++) {
    free(global_ixob[i].scratch);
  }
  free(global_ixob);
#endif  
}

/* Writes an output datum to an indexed buffer. 
 * Operates on a block of model instances in parallel.
 * quantities is an array of output data of length (outputsize * blocksize)
 * participate is an array of length blocksize indicating which threads are active
 */
__DEVICE__ void buffer_indexed_output (unsigned int modelid, unsigned int outputid, unsigned int outputsize, CDATAFORMAT *quantities, indexed_output_buffer *pos, unsigned int threadid, unsigned int blocksize, int participate) {
  unsigned int i, offset;
  CDATAFORMAT *buffer;
  indexed_sort_data *sort;
  int *index = pos->scratch;

  index[threadid] = !!participate; // ensures index is 1 or 0
  parallel_scan(index,threadid,blocksize);

  if (participate) {
    offset = pos->size + index[threadid] - 1;

    buffer = pos->buffer;
    for (i=0; i<outputsize; i++) {
      buffer[i+offset] = quantities[VEC_IDX(outputsize,i,blocksize,threadid)];
    }

    sort = pos->sort + offset;
    sort->modelid = modelid;
    sort->outputid = outputid;
    sort->offset = offset;
  }

  if (0 == threadid) {
    pos->size += index[blocksize-1] * outputsize;
  }
}
