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
 * We allocate one of these structures for each parallel block.
 * 
 * The buffer contains packed output data, and the
 * sort array comprises a set of markers identifying
 * the position of each datum.
 * scratch is a device memory pointer to a temp array of length blocksize
 */
typedef struct {
  size_t size;
  int *scratch;
  CDATAFORMAT buffer[BUFFER_LEN];
  indexed_sort_data sort[2*BUFFER_LEN];
} indexed_output_buffer;

__DEVICE__ void init_output_buffer(output_buffer *ob, unsigned int modelid);

/* Destructively computes the prefix sum of an integer vector of size length.
 * Assumes length is a power of 2. 
 */
__DEVICE__ void parallel_scan(int *vector, unsigned int threadid, unsigned int length);

indexed_output_buffer *alloc_indexed_output_buffer (unsigned int gridsize);
__DEVICE__ void init_indexed_output_buffer (unsigned int gridsize);
void free_indexed_output_buffer (indexed_output_buffer *buffer);

/* Writes an output datum to an indexed buffer. 
 * Operates on a block of model instances in parallel.
 * quantities is an array of output data of length (outputsize * blocksize)
 * participate is an array of length blocksize indicating which threads are active
 * index is an array of length blocksize used for scratch space
 */
__DEVICE__ void buffer_indexed_output (unsigned int modelid, unsigned int outputid, unsigned int outputsize, CDATAFORMAT *quantities, indexed_output_buffer *pos, unsigned int threadid, unsigned int blocksize, int participate);
