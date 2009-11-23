// http://forums.nvidia.com/index.php?showtopic=52806&pid=287558&mode=threaded&start=#entry287558

#include <stdlib.h>
#include <stdio.h>

#include <cuda_runtime_api.h>


const int num_threads = 64;
const int len = 30720 * 128;//2949120;
const int stride = 16;
int nIters = 500;



#  define CUDA_SAFE_CALL( call) do {					\
	cudaError err = call;						\
	if( cudaSuccess != err) {					\
	    fprintf(stderr, "Cuda error in file '%s' in line %i : %s.\n", \
		__FILE__, __LINE__, cudaGetErrorString( err) );		\
	    exit(EXIT_FAILURE);						\
	    } } while (0)

#ifdef NDEBUG
#define CUT_CHECK_ERROR(errorMessage)
#else
#  define CUT_CHECK_ERROR(errorMessage) do {				\
	cudaThreadSynchronize();					\
	cudaError_t err = cudaGetLastError();				\
	if( cudaSuccess != err) {					\
	    fprintf(stderr, "Cuda error: %s in file '%s' in line %i : %s.\n", \
		errorMessage, __FILE__, __LINE__, cudaGetErrorString( err) ); \
	    exit(EXIT_FAILURE);						\
	    } } while (0)
#endif



// This is a proposed new arrangement: array of structs of arrays of structs
// (ASAS.)
typedef struct {
    float4 abcd[stride];
    float4 efgh[stride];
    } states;
    
__global__ void read_and_write_states(states *y0, states *y1)
    {
    // x-axis threads are contiguous (?)
    const unsigned int modelx = threadIdx.x + blockIdx.x * blockDim.x;
    const unsigned int modely = threadIdx.y + blockIdx.y * blockDim.y;
    float4 abcd = y0[modely].abcd[modelx];
    float4 efgh = y0[modely].efgh[modelx];

    abcd.x = 1.0f * modelx;
    abcd.y = 1.0f * modely;
    efgh.z = -1.0f * modelx;
    efgh.w = -1.0f * modely;

    y1[modely].abcd[modelx] = abcd;
    y1[modely].efgh[modelx] = efgh;
    }


typedef struct {
    float2 ab[stride];
    float2 cd[stride];
    float2 ef[stride];
    float2 gh[stride];
    } states4;
    
__global__ void read_and_write_states4(states4 *y0, states4 *y1)
    {
    // x-axis threads are contiguous (?)
    const unsigned int modelx = threadIdx.x + blockIdx.x * blockDim.x;
    const unsigned int modely = threadIdx.y + blockIdx.y * blockDim.y;
    float2 ab = y0[modely].ab[modelx];
    float2 cd = y0[modely].cd[modelx];
    float2 ef = y0[modely].ef[modelx];
    float2 gh = y0[modely].gh[modelx];

    ab.x = 1.0f * modelx;
    ab.y = 1.0f * modely;
    ef.x = -1.0f * modelx;
    ef.y = -1.0f * modely;

    y1[modely].ab[modelx] = ab;
    y1[modely].cd[modelx] = cd;
    y1[modely].ef[modelx] = ef;
    y1[modely].gh[modelx] = gh;
    }


// This is the way we are arranging device memory currently.
typedef struct {
    float a[len];
    float b[len];
    float c[len];
    float d[len];
    float e[len];
    float f[len];
    float g[len];
    float h[len];
    } states2;

__global__ void read_and_write_states2(states2 *y0, states2 *y1)
    {
    const unsigned int model = threadIdx.x + blockIdx.x * blockDim.x;
    float a = y0->a[model];
    float b = y0->b[model];
    float c = y0->c[model];
    float d = y0->d[model];
    float e = y0->e[model];
    float f = y0->f[model];
    float g = y0->g[model];
    float h = y0->h[model];

    a = 1.0f * model;
    b = 1.0f * model;
    g = -1.0f * model;
    h = -1.0 * model;

    y1->a[model] = a;
    y1->b[model] = b;
    y1->c[model] = c;
    y1->d[model] = d;
    y1->e[model] = e;
    y1->f[model] = f;
    y1->g[model] = g;
    y1->h[model] = h;
    }


int main1()
    {
    CUDA_SAFE_CALL( cudaSetDevice(0) );
    int num_models = len;
    float runTime;

    states *d_idata, *d_odata;
    CUDA_SAFE_CALL( cudaMalloc((void**)&d_idata, (num_models/stride)*sizeof(states)) );
    CUDA_SAFE_CALL( cudaMalloc((void**)&d_odata, (num_models/stride)*sizeof(states)) );


    dim3  threads(stride, num_threads/stride, 1);
    dim3  grid(1, num_models/stride/threads.y, 1);

    cudaEvent_t start, end;
    CUDA_SAFE_CALL( cudaEventCreate(&start) );
    CUDA_SAFE_CALL( cudaEventCreate(&end) );
    CUDA_SAFE_CALL( cudaEventRecord(start, 0) );

    for (int i=0; i < nIters; ++i)
	{
	read_and_write_states<<< grid, threads >>>(d_idata, d_odata);
	}

    CUDA_SAFE_CALL( cudaEventRecord(end, 0) );
    CUDA_SAFE_CALL( cudaEventSynchronize(end) );

    CUDA_SAFE_CALL( cudaEventElapsedTime(&runTime, start, end) );

    runTime /= float(nIters);
    float bandwidth = (2 * (num_models/stride)*sizeof(states)) / (runTime * 1.0e-3 * (1<<30));
    printf("Average time:% 13.4f ms\n", runTime);
    printf("Bandwidth:% 16.4f GiB/s\n\n", bandwidth);

    return 0;
    }


int main4()
    {
    CUDA_SAFE_CALL( cudaSetDevice(0) );
    int num_models = len;
    float runTime;

    states4 *d_idata, *d_odata;
    CUDA_SAFE_CALL( cudaMalloc((void**)&d_idata, (num_models/stride)*sizeof(states4)) );
    CUDA_SAFE_CALL( cudaMalloc((void**)&d_odata, (num_models/stride)*sizeof(states4)) );


    dim3  threads(stride, num_threads/stride, 1);
    dim3  grid(1, num_models/stride/threads.y, 1);

    cudaEvent_t start, end;
    CUDA_SAFE_CALL( cudaEventCreate(&start) );
    CUDA_SAFE_CALL( cudaEventCreate(&end) );
    CUDA_SAFE_CALL( cudaEventRecord(start, 0) );

    for (int i=0; i < nIters; ++i)
	{
	read_and_write_states4<<< grid, threads >>>(d_idata, d_odata);
	}

    CUDA_SAFE_CALL( cudaEventRecord(end, 0) );
    CUDA_SAFE_CALL( cudaEventSynchronize(end) );

    CUDA_SAFE_CALL( cudaEventElapsedTime(&runTime, start, end) );

    runTime /= float(nIters);
    float bandwidth = (2 * (num_models/stride)*sizeof(states4)) / (runTime * 1.0e-3 * (1<<30));
    printf("Average time:% 13.4f ms\n", runTime);
    printf("Bandwidth:% 16.4f GiB/s\n\n", bandwidth);

    return 0;
    }


int main()
    {
    CUDA_SAFE_CALL( cudaSetDevice(0) );
    int num_models = len;
    float runTime;

    states2 *d_idata, *d_odata;
    CUDA_SAFE_CALL( cudaMalloc((void**)&d_idata, sizeof(states2)) );
    CUDA_SAFE_CALL( cudaMalloc((void**)&d_odata, sizeof(states2)) );


    dim3  threads(num_threads, 1, 1);
    dim3  grid(num_models/threads.x, 1, 1);

    cudaEvent_t start, end;
    CUDA_SAFE_CALL( cudaEventCreate(&start) );
    CUDA_SAFE_CALL( cudaEventCreate(&end) );
    CUDA_SAFE_CALL( cudaEventRecord(start, 0) );

    for (int i=0; i < nIters; ++i)
	{
	read_and_write_states2<<< grid, threads >>>(d_idata, d_odata);
	}

    CUDA_SAFE_CALL( cudaEventRecord(end, 0) );
    CUDA_SAFE_CALL( cudaEventSynchronize(end) );

    CUDA_SAFE_CALL( cudaEventElapsedTime(&runTime, start, end) );

    runTime /= float(nIters);
    float bandwidth = (2 * sizeof(states2)) / (runTime * 1.0e-3 * (1<<30));
    printf("Average time:% 13.4f ms\n", runTime);
    printf("Bandwidth:% 16.4f GiB/s\n\n", bandwidth);

    return 0;
    }

