// Dynamo Floating Point Operation Definitions
// Copyright 2007 Simatra Modeling Technologies

#include <math.h>
#include <stdlib.h>

inline double cr_mul(double x, double y)
{
  return x * y;
}

inline double cr_div(double x, double y)
{
  return x / y;
}

inline double cr_inv(double x)
{
  return 1.0 / x;
}

inline double cr_add(double x, double y)
{
  return x + y;
}

inline double cr_sub(double x, double y)
{
  return x - y;
}

inline double cr_le(double x, double y)
{ 
  return x <= y;
}

inline double cr_lt(double x, double y)
{ 
  return x < y;
}

inline double cr_ge(double x, double y)
{ 
  return x >= y;
}

inline double cr_gt(double x, double y)
{ 
  return x > y;
}

inline double cr_eq(double x, double y)
{
  return x == y;
}

inline double cr_neq(double x, double y)
{
  return x != y;
}

inline double cr_IF(double x, double y, double z)
{ 
  return (x) ? y : z ;
}

inline double cr_not(double x)
{
  return cr_IF(x, 0.0, 1.0);
}

inline double cr_or(double x, double y)
{
  return cr_IF(x, x, y);
}

inline double cr_and(double x, double y)
{
  return cr_IF(x, y, 0.0);
}

inline double cr_neg(double x)
{
  return cr_sub(0,x);
}

// x is a dummy variable used to prevent the optimizer from eliminating calls to the rng
double cr_rng(double x, double start, double stop, double step) {
  double low = start < stop ? start : stop;
  double high = stop > start ? stop : start;
  return low + fmod(step * rand(), high - low);
}

double cr_srng(double x, double start, double stop, double step, double seed) {
  static double sseed = 0;
  if (!sseed) {
    srand(seed);
    sseed = seed;
  }
  return cr_rng(x,start,stop,step);
}
