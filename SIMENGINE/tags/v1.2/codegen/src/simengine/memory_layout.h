// MEMORY INDEXING MODES
//****************************************************************************//
// Parallel Structure of Arrays indexing
#define SA_IDX(STRUCT_S, ARRAY_S, STRUCT_X, ARRAY_X) ((STRUCT_X) * (ARRAY_S) + (ARRAY_X))
// Parallel Array of Structures indexing
#define AS_IDX(STRUCT_S, ARRAY_S, STRUCT_X, ARRAY_X) ((ARRAY_X) * (STRUCT_S) + (STRUCT_X))
// Serial indexing
#define SER_IDX(STRUCT_S, ARRAY_S, STRUCT_X, ARRAY_X) ((STRUCT_X))
//****************************************************************************//

// Matrix indexing
#define MAT_IDX(NUM_ROWS, NUM_COLS, ROW, COL, PARALLEL_MODELS, MODELID) \
  (TARGET_IDX((NUM_ROWS*NUM_COLS), PARALLEL_MODELS, (ROW*NUM_COLS+COL), MODELID))

// Vector indexing
#define VEC_IDX(NUM_COLS, COL, PARALLEL_MODELS, MODELID) \
  (TARGET_IDX(NUM_COLS, PARALLEL_MODELS, COL, MODELID))
