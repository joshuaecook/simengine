__DEVICE__ inline int exp2i(int i){
  return 1<<i;
}

/* Destructively computes the prefix sum of an integer vector of size length. */
__DEVICE__ void parallel_scan(int *vector, unsigned int threadid, unsigned int length) {
  unsigned int i, j;
  int participate;

  for (i=0; exp2i(i)<length; i++) {
    participate = threadid >= exp2i(i);
    if (participate) {
      vector[threadid] += vector[threadid-exp2i(i)];
    }
  }
}
