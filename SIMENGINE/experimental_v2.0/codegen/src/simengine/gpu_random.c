#ifdef TARGET_GPU
/* A CUDA implementation of the R250 PRNG.
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
#define R250_LENGTH 250
#define R250_OFFSET 103
#define R250_MAX RAND_MAX

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
__DEVICE__ unsigned int gpu_r250_buffer[PARALLEL_MODELS * R250_LENGTH];
// Indexes into the above buffer.
__DEVICE__ unsigned int gpu_r250_position[PARALLEL_MODELS];

// Buffers intermediate results of computing a gaussian distribution.
__DEVICE__ CDATAFORMAT gpu_gaussian_buffer[PARALLEL_MODELS];

__HOST__ void gpu_random_init (unsigned int instances, unsigned int seed)
    {
    unsigned int *init_buffer;
    unsigned int *g_buffer;
    unsigned int *g_position;
    CDATAFORMAT *g_gaussian_buffer;
    unsigned int maskA, maskB;
    unsigned int i, pos;

    init_buffer = (unsigned int *)malloc(instances * R250_LENGTH * sizeof(unsigned int));
    srand(seed);

    for (i = 0; i < instances; i++)
	{
	maskA = 1;
	maskB = R250_MAX;
	pos = R250_LENGTH;

	while (pos-- > 31)
	    {
	    init_buffer[pos * instances + i] = rand();
	    }

	// I believe my reference contained a bug in the following
	// section by shifting in the opposite direction. 
	// Additionally, the original seems to assume that rand()
	// produces 32 bits of randomness when it only provides 31
	// bits. -Josh

	// Original author's comment: Establish linear independence of
	// the bit columns by setting the diagonal bits and clearing
	// all bits above.
	while (pos-- > 0)
	    {
	    init_buffer[pos * instances + i] = (rand() | maskA) & maskB;
	    maskB ^= maskA;
	    maskA <<= 1;
	    }
	init_buffer[i] = 0;
	}

    cutilSafeCall(cudaGetSymbolAddress((void **)&g_gaussian_buffer, gpu_gaussian_buffer));
    cutilSafeCall(cudaGetSymbolAddress((void **)&g_position, gpu_r250_position));
    cutilSafeCall(cudaGetSymbolAddress((void **)&g_buffer, gpu_r250_buffer));

    cutilSafeCall(cudaMemcpy(g_buffer, init_buffer, instances * R250_LENGTH * sizeof(unsigned int), cudaMemcpyHostToDevice));
    cutilSafeCall(cudaMemset(g_position, 0, instances * sizeof(unsigned int)));
    // Cleverly fills the gaussian buffer with a pattern of bytes that
    // will appear as an invalid value in floating point.
    cutilSafeCall(cudaMemset(g_gaussian_buffer, R250_INVALID_BYTE, instances * sizeof(CDATAFORMAT)));
    }

// Returns a uniformly-distributed random number on the interval [0,1)
__DEVICE__ CDATAFORMAT gpu_uniform_random (unsigned int instances, unsigned int instanceId)
    {
    // My reference did some clever prescaling of buffer offsets which
    // seemed to cause problems on the GPU. The justification for the
    // cleverness was wrt optimizing for PPC, so I assume that it is
    // not particularly optimal for GPU. -Josh
    unsigned int *buffer = gpu_r250_buffer + instanceId;
    unsigned int *tmp;
    unsigned int r;
    unsigned int i = gpu_r250_position[instanceId];
    int j = i - R250_OFFSET;
    if (j < 0) j = j + R250_LENGTH;

    tmp = buffer + (i * instances);
    r = *(buffer + (j * instances));
    r = r ^ *tmp;
    *tmp = r;

    gpu_r250_position[instanceId] += (i == R250_LENGTH - 1) ? -i : 1;
    
    return r / (FLITERAL(1.) * R250_MAX);
    }


// Returns a normally-distributed random number centered at 0 on the interval (-Inf, Inf)
__DEVICE__ CDATAFORMAT gpu_gaussian_random (unsigned int instances, unsigned int instanceId)
    {
    CDATAFORMAT grandom = gpu_gaussian_buffer[instanceId];
    CDATAFORMAT x1, x2;
    CDATAFORMAT w;

    if (R250_IS_VALID(&grandom))
	{
	// Return the previously-computed value and invalidate the
	// stored buffer.
	R250_INVALIDATE(gpu_gaussian_buffer + instanceId);
	}
    else
	{
	// Compute 2 new values and hold one
	w = FLITERAL(1.);
	do
	    {
	    x1 = 2 * gpu_uniform_random(instances, instanceId) - 1;
	    x2 = 2 * gpu_uniform_random(instances, instanceId) - 1;
	    w = x1 * x1 + x2 * x2;
	    } while (w >= FLITERAL(1.));

	w = pow(FLITERAL(-2.) * log(w) / w, FLITERAL(0.5));
	grandom = x1 * w;
	gpu_gaussian_buffer[instanceId] = x2 * w;
	}

    return grandom;
    }

#endif
