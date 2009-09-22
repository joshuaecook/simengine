#include "simengine.h"


#define ITERSPACE 1
#define INTEGRATION_METHOD dormand_prince
#define INTEGRATION_MEM dormand_prince_mem
#define START_SIZE 1000
#define MAX_ALLOC_SIZE 65536000
#define OUTPUT_BUFFER_LENGTH 8000
#define MAX_ITERATIONS 128
#define DT 0.5E-1
#define ABSTOL 0.1E-5
#define RELTOL 0.1E-2

const char *input_names[] = {"gNa", "gCaT", "gCaS", "gA", "gKCa", "gKd", "gh", "gleak"};
const char *state_names[] = {"V", "mNa", "hNa", "mCaT", "hCaT", "mCaS", "hCaS", "mA", "hA", "mKCa", "mKd", "mh", "Caconc"};
const char *output_names[] = {"Vm"};
const double default_inputs[] = {FLITERAL(0.2E3), FLITERAL(0.1E2), FLITERAL(0.1E2), FLITERAL(0.0), FLITERAL(0.0), FLITERAL(0.25E2), FLITERAL(0.1E-1), FLITERAL(0.5E-1)};
const double default_states[] = {FLITERAL(-0.575E2), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5), FLITERAL(0.5E-1)};
const unsigned int output_num_quantities[] = {2};
const char model_name[] = "stg_spiker";
const char solver[] = "dormand_prince";
#if defined TARGET_CPU
const char target[] = "cpu";
#elif defined TARGET_OPENMP
const char target[] = "openmp";
#elif defined TARGET_GPU
const char target[] = "gpu";
#endif

const simengine_metadata semeta = {
  0x0000000000000000ULL, // hashcode
  NUM_MODELS,
  solver,
  target,
  sizeof(CDATAFORMAT)
};

#define NUM_INPUTS 8
#define NUM_STATES 13
#define NUM_OUTPUTS 1

const simengine_interface seint = {
  0, // Version,
  NUM_INPUTS, // Number of inputs
  NUM_STATES, // Number of states
  NUM_OUTPUTS, // Number of outputs
  input_names,
  state_names,
  output_names,
  default_inputs,
  default_states,
  output_num_quantities,
  model_name,
  &semeta
};

simengine_alloc se_alloc = { malloc, realloc, free };

typedef struct {
  CDATAFORMAT mdlvar__Vm;
} output_data;

output_data OD[NUM_MODELS];

// define state structures
struct statedata_stg_spiker {
  // states (count=13)
  CDATAFORMAT mdlvar__V[ARRAY_SIZE];
  CDATAFORMAT mdlvar__mNa[ARRAY_SIZE];
  CDATAFORMAT mdlvar__hNa[ARRAY_SIZE];
  CDATAFORMAT mdlvar__mCaT[ARRAY_SIZE];
  CDATAFORMAT mdlvar__hCaT[ARRAY_SIZE];
  CDATAFORMAT mdlvar__mCaS[ARRAY_SIZE];
  CDATAFORMAT mdlvar__hCaS[ARRAY_SIZE];
  CDATAFORMAT mdlvar__mA[ARRAY_SIZE];
  CDATAFORMAT mdlvar__hA[ARRAY_SIZE];
  CDATAFORMAT mdlvar__mKCa[ARRAY_SIZE];
  CDATAFORMAT mdlvar__mKd[ARRAY_SIZE];
  CDATAFORMAT mdlvar__mh[ARRAY_SIZE];
  CDATAFORMAT mdlvar__Caconc[ARRAY_SIZE];
  // instances (count=0)
};

#define MAX_OUTPUT_SIZE (NUM_OUTPUTS*2*sizeof(int) + (NUM_OUTPUTS+1)*sizeof(CDATAFORMAT)) //size in bytes

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
typedef struct{
  unsigned int active_models;
  unsigned int finished[NUM_MODELS];
  unsigned int full[NUM_MODELS];
  unsigned int count[NUM_MODELS];
  unsigned int vb_count[NUM_MODELS];
  CDATAFORMAT buffer[OUTPUT_BUFFER_LENGTH*NUM_MODELS];
  void *ptr[NUM_MODELS];
  void *end[NUM_MODELS];
} output_buffer;
