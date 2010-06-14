// DSL Standard Library
// Copyright 2009 Simatra Modeling Technologies

#ifndef _DSLLIB_H
#define _DSLLIB_H

#include <math.h>

// handle abs
int abs(int x) {
  return abs(x);
}
long abs(long x) {
  return abs(x);
}

float abs(float x) {
  return fabs(x);
}
double abs(double x) {
  return fabs(x);
}

// radian/degree conversion
double rad2deg(double r) {
  return r * 180/M_PI;
}
double deg2rad(double d) {
  return d * M_PI/180;
}

#endif /* dsllib.h */
