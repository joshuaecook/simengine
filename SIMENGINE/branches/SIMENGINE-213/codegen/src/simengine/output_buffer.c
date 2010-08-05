indexed_output_buffer *global_ixob = NULL;
#if defined TARGET_GPU
__DEVICE__ *gpu_ixob = NULL;
#endif


__DEVICE__ void init_output_buffer(output_buffer *ob, unsigned int modelid){
  ob->full[modelid] = 0;
  ob->count[modelid] = 0;
  ob->ptr[modelid] = &ob->buffer[modelid*BUFFER_LEN];
  ob->end[modelid] = &ob->buffer[(modelid+1)*BUFFER_LEN];
}

/* Destructively computes the prefix sum of an integer vector of size length.
 * Assumes length is a power of 2. 
 */
__DEVICE__ void parallel_scan(int *vector, unsigned int threadid, unsigned int length) {
  unsigned int stride;
  int participate;

  for (stride=2; stride<=length; stride*=2) {
    participate = !((threadid+1) & (stride-1));
    if (participate) {
      vector[threadid] += vector[threadid-(stride/2)];
    }
  }
}

/* Writes an output datum to an indexed buffer. 
 * Operates on a block of model instances in parallel.
 * quantities is an array of output data of length (outputsize * blocksize)
 * participate is an array of length blocksize indicating which threads are active
 */
__SHARED__ int *buffer_indexed_output_scratch;
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
