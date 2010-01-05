#include<stdlib.h>

// Produce a uniform random number on the interval [0,1)
CDATAFORMAT uniform_random_unsafe(){
  return drand48();
}

CDATAFORMAT uniform_random(){
  CDATAFORMAT r;
  // Ensure thread safety
#pragma omp critical
  {
    r = uniform_random_unsafe();
  }
  return r;
}

// Produce a Gaussian random number with mean of 1
CDATAFORMAT gaussian_random(){
  static CDATAFORMAT bgrandom;
  static int available;

  CDATAFORMAT grandom;

#pragma omp critical
  {
    // Ensure thread safety
    CDATAFORMAT w = 1;
    CDATAFORMAT x1;
    CDATAFORMAT x2;

    // Return a previously buffered random value
    if(available){
      grandom = bgrandom;
      available = 0;
    }
    // Compute two new random values and buffer one
    else{
      while(w>=1){
	x1 = 2*uniform_random_unsafe()-1;
	x2 = 2*uniform_random_unsafe()-1;
	w = x1*x1 + x2*x2;
      }
      w = sqrt(-2*log(w)/w);
      x1 *= w;
      x2 *= w;
      grandom = x1;
      available = 1;
      bgrandom = x2;
    }
  }
  return grandom;
}
