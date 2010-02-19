#ifdef TARGET_GPU
/* A CUDA implementation of the R250 PRNG.
 *
 * Each random bit X is produced by the sum of 2 past bits
 *   X[k] = X[k-q] + K[k-p]
 * where q = 103 and p = 250. That is, a generalized feedback shift
 * register GFSR(250,103).
 *
 * Uses 32 bits of entropy to produce a random value with a period of
 * 2 ** 249. This algorithm requires another source of randomness to
 * seed its buffer which retains the last 250 generated values.
 *
 * Based on a public domain implementation by Michael Brundage.
 * The reference was optimized for PPC, so this implementation
 * could possibly be improved wrt GPU instruction sets.
 *
 * See http://www.taygeta.com/rwalks/node2.html
 * See http://www.qbrundage.com/michaelb/pubs/essays/random_number_generation.html
 */
#define R250_LENGTH 250
#define R250_MAX 0xFFFFFFFFUL

#define R250_Q (103 * sizeof(unsigned long))
#define R250_P (R250_LENGTH * sizeof(unsigned long) - R250_Q)

__DEVICE__ unsigned long gpu_r250_buffer[PARALLEL_MODELS * R250_LENGTH];
__DEVICE__ unsigned int gpu_r250_position[PARALLEL_MODELS];

__DEVICE__ CDATAFORMAT gpu_gaussian_buffer[PARALLEL_MODELS];

__HOST__ void gpu_random_init (unsigned int instances)
    {
    unsigned long *init_buffer;
    unsigned long *g_buffer;
    unsigned int *g_position;
    CDATAFORMAT *g_gaussian_buffer;
    unsigned long maskA, maskB;
    unsigned int i, pos;

    init_buffer = (unsigned long *)malloc(instances * R250_LENGTH * sizeof(unsigned long));

    for (i = 0; i < instances; i++)
	{
	maskA = 1;
	maskB = R250_MAX;
	pos = R250_LENGTH;

	while (pos-- > 31)
	    {
	    init_buffer[pos * instances + i] = rand();
	    }

	// I believe my reference implementation contained a bug in
	// the following section by shifting in the opposite
	// direction. -Josh
	// Original author's comment: Establish linear independence of
	// the bit columns by setting the diagonal bits and clearing
	// all bits above.
	while (pos-- > 0)
	    {
	    init_buffer[pos * instances + i] = (rand() | maskA) & maskB;
	    maskB ^= maskA;
	    maskA <<= 1;
	    }
	init_buffer[0] = maskA;
	}

    cutilSafeCall(cudaGetSymbolAddress((void **)&g_gaussian_buffer, gpu_gaussian_buffer));
    cutilSafeCall(cudaGetSymbolAddress((void **)&g_position, gpu_r250_position));
    cutilSafeCall(cudaGetSymbolAddress((void **)&g_buffer, gpu_r250_buffer));

    cutilSafeCall(cudaMemcpy(g_buffer, init_buffer, instances * R250_LENGTH * sizeof(unsigned long), cudaMemcpyHostToDevice));
    cutilSafeCall(cudaMemset(g_position, 0, instances * sizeof(unsigned int)));
    // Cleverly fills the gaussian buffer with a pattern of bytes that
    // will appear as negative values in floating point. Any byte in
    // the range 0x80 to 0xFE will work.
    cutilSafeCall(cudaMemset(g_gaussian_buffer, 0x80, instances * sizeof(CDATAFORMAT)));
    }

// Returns a uniformly-distributed random number on the interval [0,1)
__DEVICE__ CDATAFORMAT gpu_uniform_random (unsigned int instances, unsigned int instanceId)
    {
    unsigned char *buffer = (unsigned char *)gpu_r250_buffer;
    unsigned long *tmp;
    unsigned long r;
    unsigned int i = gpu_r250_position[instanceId];
    int j = i - R250_P;
    if (j < 0) j = i + R250_P;

    tmp = (unsigned long *)(buffer + (i * instances) + instanceId);
    r = *(unsigned long *)(buffer + (j * instances) + instanceId);
    r = r ^ *tmp;
    *tmp = r;

    gpu_r250_position[instanceId] += (i == (R250_LENGTH - 1) * sizeof(unsigned long)) ? -i : sizeof(unsigned long);

    return r / (FLITERAL(1.) * R250_MAX);
    }



__DEVICE__ CDATAFORMAT gpu_gaussian_random (unsigned int instances, unsigned int instanceId)
    {
    CDATAFORMAT grandom = gpu_gaussian_buffer[instanceId];
    CDATAFORMAT x1, x2;
    CDATAFORMAT w;

    if (grandom < FLITERAL(0.))
	{
	// Compute 2 new values and hold one
	w = FLITERAL(1.);
	do
	    {
	    x1 = 2 * gpu_uniform_random(instances, instanceId) - 1;
	    x2 = 2 * gpu_uniform_random(instances, instanceId) - 1;
	    w = x1 * x1 + x2 * x2;
	    } while (w >= FLITERAL(1.));
	w = FLITERAL(1.) / rsqrt(FLITERAL(-2.) * log(w) / w);
	grandom = x1 * w;
	gpu_gaussian_buffer[instanceId] = x2 * w;
	}
    else
	{
	// Return the previously-computed value
	gpu_gaussian_buffer[instanceId] = FLITERAL(-1.);
	}

    return grandom;
    }

#endif
