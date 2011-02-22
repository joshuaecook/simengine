// Copyright (c) 2010 Simatra Modeling Technologies, L.L.C.
#include <omp.h>

#include "ffi-exports.h"

Int32_t openmp_getNumProcessors(){
  omp_get_num_threads(); // For some reason this call forces gcc to properly link all openmp libraries.
                         // without it, Error "Undefined symbols: _gomp_thread_attr" appears on OS X.
                         // This makes no sense, but if it makes it work.....
  return omp_get_num_procs();
}
