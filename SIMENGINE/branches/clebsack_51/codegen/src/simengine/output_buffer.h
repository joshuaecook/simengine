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
#ifdef NUM_MODELS
#define BUFFER_LEN 8000
typedef struct{
  unsigned int active_models;
  unsigned int finished[NUM_MODELS];
  unsigned int full[NUM_MODELS];
  unsigned int count[NUM_MODELS];
  void *ptr[NUM_MODELS];
  void *end[NUM_MODELS];
  CDATAFORMAT buffer[BUFFER_LEN*NUM_MODELS];
} output_buffer;
#endif
