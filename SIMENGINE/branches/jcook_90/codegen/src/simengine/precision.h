// The type of simulation quantity values.
#if defined SIMENGINE_STORAGE_float
#define SIMENGINE_STORAGE float
typedef float CDATAFORMAT;
// Ensures that operations involving literal quantities are not promoted to double-precision.
#define FLITERAL(X) X##f
#elif defined SIMENGINE_STORAGE_double
// Default to double precision
#define SIMENGINE_STORAGE double
typedef double CDATAFORMAT;
#define FLITERAL(X) X
#else
#error Must define a storage type (SIMENGINE_STORAGE_float or SIMENGINE_STORAGE_double)
#endif
