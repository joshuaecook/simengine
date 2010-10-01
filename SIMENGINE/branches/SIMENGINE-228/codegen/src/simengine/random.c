/* A parallel implementation of the R250 PRNG.
 *
 * Each random bit X is produced by the sum of 2 past bits
 *   X[k] = X[k-q] + K[k-p]
 * where q = 103 and p = 250. That is, a generalized feedback shift
 * register GFSR(250,103).
 *
 * Uses 31 bits of entropy to produce a random value with a period of
 * 2 ** 249. This algorithm requires another source of randomness to
 * seed its buffer which retains the last 250 generated values.
 *
 * Based on a (buggy) public domain implementation by Michael Brundage.
 *
 * See http://www.taygeta.com/rwalks/node2.html
 * See http://www.qbrundage.com/michaelb/pubs/essays/random_number_generation.html
 */
/* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. */

#define R250_LENGTH 250
#define R250_OFFSET 103
#define R250_MAX RAND_MAX

// An invalid data marker the same size as valid data.
#ifdef SIMENGINE_STORAGE_float
typedef unsigned int r250_invalid;
const r250_invalid R250_INVALID = 0xFFFFFFFFUL;
#else
typedef unsigned long long r250_invalid;
const r250_invalid R250_INVALID = 0xFFFFFFFFFFFFFFFFULL;
#endif

const unsigned char R250_INVALID_BYTE = 0xFF;

#define R250_IS_VALID(ADDR) (R250_INVALID ^ *((r250_invalid *)(ADDR)))
#define R250_INVALIDATE(ADDR) (*((r250_invalid *)(ADDR)) = R250_INVALID)

// The initial seed and memo of previously computed random values.
__DEVICE__ unsigned int r250_buffer[PARALLEL_MODELS * R250_LENGTH];
// Indexes into the above buffer.
__DEVICE__ unsigned int r250_position[PARALLEL_MODELS];
// Buffers intermediate results of computing a gaussian distribution.
__DEVICE__ CDATAFORMAT gaussian_buffer[PARALLEL_MODELS];
#ifdef TARGET_GPU
// Host memory copies of the above data.
unsigned int h_r250_buffer[PARALLEL_MODELS * R250_LENGTH];
unsigned int h_r250_position[PARALLEL_MODELS];
CDATAFORMAT h_gaussian_buffer[PARALLEL_MODELS];
#endif

// Invokes the PRNG with the appropriate state addresses
#define DEVICE_UNIFORM_RANDOM(N, I) (uniform_random(N, I, r250_buffer, r250_position))
#define DEVICE_NORMAL_RANDOM(N, I) (gaussian_random(N, I, gaussian_buffer, r250_buffer, r250_position))
#ifdef TARGET_GPU
#define HOST_UNIFORM_RANDOM(N, I) (uniform_random(N, I, h_r250_buffer, h_r250_position))
#define HOST_NORMAL_RANDOM(N, I) (gaussian_random(N, I, h_gaussian_buffer, h_r250_buffer, h_r250_position))
#else
#define HOST_UNIFORM_RANDOM DEVICE_UNIFORM_RANDOM
#define HOST_NORMAL_RANDOM DEVICE_NORMAL_RANDOM
#endif

void seed_entropy (unsigned int seed) {
  srand(seed);
}

void seed_entropy_with_time (void) {
  struct timeval tv;
  if (0 != gettimeofday(&tv, NULL)){
    ERROR(Simatra:PRNG, "Failed while getting current time: %s.", strerror(errno));
  }
  seed_entropy(tv.tv_sec);
}

void random_init_instance (unsigned int instanceId, unsigned int *init_buffer) {
  unsigned int maskA, maskB;
  unsigned int pos;

  maskA = 1;
  maskB = R250_MAX;
  pos = R250_LENGTH;

  while (pos-- > 31) {
    init_buffer[VEC_IDX(R250_LENGTH, pos, PARALLEL_MODELS, instanceId)] = rand();
  }

  // I believe my reference contained a bug in the following
  // section by shifting in the opposite direction. 
  // Additionally, the original seems to assume that rand()
  // produces 32 bits of randomness when it only provides 31
  // bits. -Josh

  // Original author's comment: Establish linear independence of
  // the bit columns by setting the diagonal bits and clearing
  // all bits above.
  while (pos-- > 0) {
    init_buffer[VEC_IDX(R250_LENGTH, pos, PARALLEL_MODELS, instanceId)] = (rand() | maskA) & maskB;
    maskB ^= maskA;
    maskA <<= 1;
  }
  init_buffer[VEC_IDX(R250_LENGTH, 0, PARALLEL_MODELS, instanceId)] = 0;
}

void random_init (unsigned int instances) {
  unsigned int *init_buffer;
  unsigned int *init_position;
  CDATAFORMAT *gaussian_init;
#ifdef TARGET_GPU
  init_buffer = h_r250_buffer;
  init_position = h_r250_position;
  gaussian_init = h_gaussian_buffer;
#else
  init_buffer = r250_buffer;
  init_position = r250_position;
  gaussian_init = gaussian_buffer;
#endif

  unsigned int i;
  for (i = 0; i < instances; i++) {
    random_init_instance(i, init_buffer);
  }

  memset(init_position, 0, PARALLEL_MODELS * sizeof(unsigned int));
  // Cleverly fills the gaussian buffer with a pattern of bytes that
  // will appear as an invalid value in floating point.
  memset(gaussian_init, R250_INVALID_BYTE, PARALLEL_MODELS * sizeof(CDATAFORMAT));
}

// Copies the current state of the PRNG to device memory.
// No op for CPU-based targets.
void random_copy_state_to_device (void) {
#ifdef TARGET_GPU
  unsigned int *g_buffer;
  unsigned int *g_position;
  CDATAFORMAT *g_gaussian_buffer;
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_gaussian_buffer, gaussian_buffer));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_position, r250_position));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_buffer, r250_buffer));

  cutilSafeCall(cudaMemcpy(g_buffer, h_r250_buffer, PARALLEL_MODELS * R250_LENGTH * sizeof(unsigned int), cudaMemcpyHostToDevice));
  cutilSafeCall(cudaMemcpy(g_position, h_r250_position, PARALLEL_MODELS * sizeof(unsigned int), cudaMemcpyHostToDevice)); 
  cutilSafeCall(cudaMemcpy(g_gaussian_buffer, h_gaussian_buffer, PARALLEL_MODELS * sizeof(CDATAFORMAT), cudaMemcpyHostToDevice));
#endif
}

// Copies the current state of the PRNG from device memory.
// No op for CPU-based targets
void random_copy_state_from_device (void) {
#ifdef TARGET_GPU
  unsigned int *g_buffer;
  unsigned int *g_position;
  CDATAFORMAT *g_gaussian_buffer;
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_gaussian_buffer, gaussian_buffer));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_position, r250_position));
  cutilSafeCall(cudaGetSymbolAddress((void **)&g_buffer, r250_buffer));

  cutilSafeCall(cudaMemcpy(h_r250_buffer, g_buffer, PARALLEL_MODELS * R250_LENGTH * sizeof(unsigned int), cudaMemcpyDeviceToHost));
  cutilSafeCall(cudaMemcpy(h_r250_position, g_position, PARALLEL_MODELS * sizeof(unsigned int), cudaMemcpyDeviceToHost)); 
  cutilSafeCall(cudaMemcpy(h_gaussian_buffer, g_gaussian_buffer, PARALLEL_MODELS * sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));
#endif
}

// Returns a uniformly-distributed random number on the interval [0,1)
__HOST__ __DEVICE__ CDATAFORMAT uniform_random (unsigned int instances, unsigned int instanceId, unsigned int *buffer, unsigned int *position) {
  // My reference did some clever prescaling of buffer offsets which
  // seemed to cause problems on the GPU. The justification for the
  // cleverness was wrt optimizing for PPC, so I assume that it is
  // not particularly optimal for GPU or x86. We use our
  // target-agnostic parallel indexing macros. -Josh
  unsigned int *tmp;
  unsigned int r;
  unsigned int i = position[instanceId];
  int j = i - R250_OFFSET;
  if (j < 0) j = j + R250_LENGTH;

  tmp = buffer + VEC_IDX(R250_LENGTH, i, instances, instanceId);
  r = buffer[VEC_IDX(R250_LENGTH, j, instances, instanceId)];
  r = r ^ *tmp;
  *tmp = r;

  position[instanceId] += (i == R250_LENGTH - 1) ? -i : 1;
    
  return r / (FLITERAL(1.) * R250_MAX);
}

// We previously relied on the gaussian transform implemented below, which was shown to be slower both on the CPU and the GPU than Box Muller,
// although the discrepancy is much more profound on the GPU
//#define gaussian_random gaussian_transform
#define gaussian_random box_muller_transform

// Returns a normally-distributed random number centered at 0 on the interval (-Inf, Inf)
__HOST__ __DEVICE__ CDATAFORMAT gaussian_transform (unsigned int instances, unsigned int instanceId, CDATAFORMAT *buffer, unsigned int *ubuffer, unsigned int *uposition) {
  CDATAFORMAT grandom = buffer[instanceId];
  CDATAFORMAT x1, x2;
  CDATAFORMAT w;

  if (R250_IS_VALID(&grandom)) {
    // Return the previously-computed value and invalidate the
    // stored buffer.
    R250_INVALIDATE(buffer + instanceId);
  }
  else {
    // Compute 2 new values and hold one
    w = FLITERAL(1.);
    do {
      x1 = 2 * uniform_random(instances, instanceId, ubuffer, uposition) - 1;
      x2 = 2 * uniform_random(instances, instanceId, ubuffer, uposition) - 1;
      w = x1 * x1 + x2 * x2;
    } while (w >= FLITERAL(1.));

    w = pow(FLITERAL(-2.) * log(w) / w, FLITERAL(0.5));
    grandom = x1 * w;
    buffer[instanceId] = x2 * w;
  }

  return grandom;
}


// Returns a normally-distributed random number centered at 0 on the interval (-Inf, Inf)
__HOST__ __DEVICE__ CDATAFORMAT box_muller_transform(unsigned int instances, unsigned int instanceId, CDATAFORMAT *buffer, unsigned int *ubuffer, unsigned int *uposition){
  CDATAFORMAT grandom = buffer[instanceId];
  CDATAFORMAT u0, u1;
  CDATAFORMAT r;
  CDATAFORMAT theta;
  const CDATAFORMAT PI = FLITERAL(3.14159265358979323846);

  if(R250_IS_VALID(&grandom)){
    // Return the previously-computed value and invalidate the
    // stored buffer.
    R250_INVALIDATE(buffer + instanceId);
  }
  else{
    // Compute 2 new values and hold one
    u0 = uniform_random(instances, instanceId, ubuffer, uposition);
    u1 = uniform_random(instances, instanceId, ubuffer, uposition);
    r = sqrt(FLITERAL(-2.0) * log(u0));
    theta = FLITERAL(2.0)*PI*u1;

    grandom = r*sin(theta);
    buffer[instanceId] = r*cos(theta);
  }

  return grandom;
}
