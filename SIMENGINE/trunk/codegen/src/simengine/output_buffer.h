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
