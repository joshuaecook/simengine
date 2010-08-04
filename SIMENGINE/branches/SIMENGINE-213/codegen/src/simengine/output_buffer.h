/* An internal data structure that maintains a buffer of output data.
 *
 * The 'count' array tracks the number of data produced for each model.
 *
 * The 'buffer' array comprises a list of tagged output data for each
 * model having the format:
 *     {tag, count, quantities[count]}
 * where 'tag' is an integer identifying a model output, 'count' is a
 * counter specifying the number of data quantities, and 'quantities'
 * is an array of actual data points.
 *
 * The 'ptr' and 'end' pointers are references to positions within 'buffer.'
 */
#ifndef BUFFER_LEN
#define BUFFER_LEN MAX(100, 2*MAX_OUTPUT_SIZE/sizeof(CDATAFORMAT))
#endif
typedef struct{
  unsigned int finished[PARALLEL_MODELS];
  unsigned int full[PARALLEL_MODELS];
  unsigned int count[PARALLEL_MODELS];
  unsigned int available[PARALLEL_MODELS];
  unsigned int modelid_offset[PARALLEL_MODELS];
  unsigned int ignored_alignment[PARALLEL_MODELS];
  CDATAFORMAT buffer[PARALLEL_MODELS*BUFFER_LEN];
  void *ptr[PARALLEL_MODELS];
  void *end[PARALLEL_MODELS];
} output_buffer;

typedef struct {
  unsigned int outputid;
  unsigned int num_quantities;
  CDATAFORMAT quantities[];
} output_buffer_data;


/* A marker of the sort index. */
typedef struct {
  unsigned int modelid;
  unsigned int outputid;
  unsigned int offset;
} indexed_sort_data;

/* An indexed element array.
 * The buffer contains packed output data, and the
 * sort array comprises a set of markers identifying
 * the position of each datum.
 */
typedef struct {
  size_t size;
  CDATAFORMAT buffer[BUFFER_LEN];
  indexed_sort_data sort[2*BUFFER_LEN];
} indexed_output_buffer;



/* Destructively computes the prefix sum of an integer vector of size length.
 * Assumes length is a power of 2. 
 */
__DEVICE__ void parallel_scan(int *vector, unsigned int threadid, unsigned int length) {
  int stride;
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
 * index is an array of length blocksize used for scratch space
 */
__SHARED__ int *buffer_indexed_output_scratch;
__DEVICE__ void buffer_indexed_output (unsigned int modelid, unsigned int outputid, unsigned int outputsize, CDATAFORMAT *quantities, indexed_output_buffer *pos, unsigned int threadid, unsigned int blocksize, int participate) {
  int i, offset;
  int *index = buffer_indexed_output_scratch;
  CDATAFORMAT *buffer;
  indexed_sort_data *sort;

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
