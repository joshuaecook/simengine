// Dynamo Floating Point Operation Definitions
// Copyright 2007 Simatra Modeling Technologies
#include <stdio.h>
//#include <gmp.h>
#include <math.h>
#include <float.h>

typedef struct 
{
  double v;
  double max;
  double min;
  long count;
  long underflows;
  long overflows;
  long negsats;
  double prev;
  long same; // counts the number of times the same value appears
  double min_dx; // shows the smallest (non-zero) delta.  We know the smallest change is zero if same > 0.
  double max_dx;
} floatpt;

inline double floatpt2num(floatpt fp) {
  return fp.v;
}

// Constructor
inline floatpt create_floatpt() {
  floatpt temp;

  temp.v = 0;
  temp.max = DBL_MIN;
  temp.min = DBL_MAX;
  temp.count = 0;
  temp.prev = 0;
  temp.same = 0;
  temp.min_dx = DBL_MAX;
  temp.max_dx = 0;
  return temp;
} 

// Initialize a floatpt quantity without defining a value
inline floatpt init_floatpt () {

  floatpt fp = create_floatpt();
  return fp;
}

// Generates a floatpt number and initializes it with a value
inline floatpt num2floatpt (double value) {
  floatpt fp = init_floatpt();

  fp.max = value;
  fp.min = value;
  fp.count = 1;
  fp.v = value;

  return fp;
}

void floatpt2str (char* str, floatpt fp) {
  double val = floatpt2num(fp);

  sprintf(str,"%10g",val);
}

void floatpt2stats (char* str, floatpt fp) {
  char val[50];
  floatpt2str(val, fp);
  sprintf(str,"Real=%s, Count=%d, Max=%.13g, Min=%.13g, Same=%d, MaxDx=%.13g, MinDx=%.13g", val, fp.count, fp.max, fp.min, fp.same, fp.max_dx, fp.min_dx);
}

void print_fl (floatpt fp) {
  char str[250];
  floatpt2stats(str, fp);
  fprintf(stderr,"%s\n", str);
  fflush(stderr);
}


inline void conv_floatpt(char* id, floatpt* fp_ptr) {
  
  floatpt fp = *fp_ptr;

  // update min and max
  double num = floatpt2num(fp);
  if (num > fp.max) {
    fp.max = num;
  }
  if (num < fp.min) {
    fp.min = num;
  }

  *fp_ptr = fp;

}

// realign_floatpt - updates internal value to match desired precision
void realign_floatpt(char* id, floatpt* fp) {
  floatpt temp = create_floatpt();

  // copy over min/max
  temp.min = (*fp).min;
  temp.max = (*fp).max;
  temp.v = (*fp).v;

  conv_floatpt(id, &temp);

  // copy back values
  (*fp).v = temp.v;
  (*fp).min = temp.min;
  (*fp).max = temp.max;
}

inline void update_floatpt_value(floatpt* fp, double prev) {

  // store the previous value (if at least one value has been stored)
  if (fp->count > 0) {
    fp->prev = prev;
  }

  // retrieve the new value
  double new = floatpt2num(*fp);

  // update statistics
  if (fp->count > 0) {
    if (new == prev) {
      fp->same++;
    }
    double diff = fabs(new-prev);
    if (fp->min_dx > diff && new != prev) {
      fp->min_dx = diff;
    }
    if (fp->max_dx < diff) {
      fp->max_dx = diff;
    }
  }
  
  // increment the count
  fp->count++;

} 

inline void set_floatpt_value(char* id, floatpt* fp, double value) {

  // store the previous value
  double prev = floatpt2num(*fp);

  // store the new value
  fp->v = value;

  // adjust the floating point back to the appropriate precision
  conv_floatpt(id, fp);

  // update statistics
  update_floatpt_value(fp, prev);
}

inline void asgn_floatpt(floatpt *target, floatpt *source) {

  target->min = source->min;
  target->max = source->max;
  target->prev = source->prev;
  target->v = source->v;

}

inline void asgn_floatpt_value(char* id, floatpt *target, floatpt *source) {

  // store the previous value
  double prev = floatpt2num(*target);

  // copy over the new precision
  asgn_floatpt (target, source);
  
  // run conv_floatpt to convert to the desired precision
  conv_floatpt (id, target);

  // update statistics
  update_floatpt_value(target, prev);
}

inline void fl_cast(char* id, floatpt* fl_p, floatpt fl_a) {
  asgn_floatpt_value(id, fl_p, &fl_a);
}

inline void fl_mult(char* id, floatpt* fp_p, floatpt fp_a, floatpt fp_b) {

  // store the previous value
  double prev = floatpt2num(*fp_p);

  fp_p->v = fp_a.v * fp_b.v;

  realign_floatpt(id, fp_p);

  // update statistics
  update_floatpt_value(fp_p, prev);
}

inline void fl_mul (char* id, floatpt* fp_p, floatpt fp_a, floatpt fp_b) {
  fl_mult(id, fp_p, fp_a, fp_b);
}

inline void fl_add(char* id, floatpt* fp_p, floatpt fp_a, floatpt fp_b) {
 
  // store the previous value
  double prev = floatpt2num(*fp_p);

  // perform the addition
  fp_p->v = fp_a.v + fp_b.v;

  realign_floatpt(id, fp_p);

  // update statistics
  update_floatpt_value(fp_p, prev);


}

inline void fl_sub(char* id, floatpt* fp_p, floatpt fp_a, floatpt fp_b) {
 
  // store the previous value
  double prev = floatpt2num(*fp_p);

  // perform the subtraction
  fp_p->v = fp_a.v - fp_b.v;

  realign_floatpt(id, fp_p);

  // update statistics
  update_floatpt_value(fp_p, prev);

}

inline void fl_neg(char* id, floatpt* fp_p, floatpt fp_a) {
  fl_sub(id, fp_p, num2floatpt(0.0), fp_a);
}


inline void fl_neq(char* id, floatpt* p, floatpt fp_a, floatpt fp_b) {

  // store the previous value
  double prev = floatpt2num(*p);

  // perform the comparison
  set_floatpt_value(id, p, fp_a.v != fp_b.v);

  // update statistics
  update_floatpt_value(p, prev);

}

inline void fl_equal(char* id, floatpt* p, floatpt fp_a, floatpt fp_b) {

  // store the previous value
  double prev = floatpt2num(*p);

  // perform the comparison
  set_floatpt_value(id, p, fp_a.v == fp_b.v);

  // update statistics
  update_floatpt_value(p, prev);

}

inline void fl_gt(char* id, floatpt* p, floatpt fp_a, floatpt fp_b) {

  // store the previous value
  double prev = floatpt2num(*p);

  // perform the comparison
  set_floatpt_value(id, p, fp_a.v > fp_b.v);

  // update statistics
  update_floatpt_value(p, prev);

}

inline void fl_lt(char* id, floatpt* p, floatpt fp_a, floatpt fp_b) {

  // store the previous value
  double prev = floatpt2num(*p);

  // perform the comparison
  set_floatpt_value(id, p, fp_a.v < fp_b.v);

  // update statistics
  update_floatpt_value(p, prev);

}

inline void fl_ge(char* id, floatpt* p, floatpt fp_a, floatpt fp_b) {

  // store the previous value
  double prev = floatpt2num(*p);

  // perform the comparison
  set_floatpt_value(id, p, fp_a.v >= fp_b.v);

  // update statistics
  update_floatpt_value(p, prev);

}

inline void fl_le(char* id, floatpt* p, floatpt fp_a, floatpt fp_b) {

  // store the previous value
  double prev = floatpt2num(*p);

  // perform the comparison
  set_floatpt_value(id, p, fp_a.v <= fp_b.v);

  // update statistics
  update_floatpt_value(p, prev);

}


inline void fl_if(char* id, floatpt* fp_p, floatpt fp_cond, floatpt fp_ift, floatpt fp_iff) {

  // store the previous value
  double prev = floatpt2num(*fp_p);

  // perform the if 
  fp_p->v = fp_cond.v ? fp_ift.v : fp_iff.v;

  realign_floatpt(id, fp_p);

  // update statistics
  update_floatpt_value(fp_p, prev);

}

inline void fl_not(char* id, floatpt* fp_p, floatpt fp_a) {
  fl_if(id, fp_p, fp_a, num2floatpt(0.0), num2floatpt(1.0));
}

inline void fl_or(char* id, floatpt* fp_p, floatpt fp_a, floatpt fp_b) {
  fl_if(id, fp_p, fp_a, fp_a, fp_b);
}

inline void fl_and(char* id, floatpt* fp_p, floatpt fp_a, floatpt fp_b) {
    fl_if(id, fp_p, fp_a, fp_b, num2floatpt(0.0));
}

// Operations that are not normally supported in hardware will use double approximations
inline void fl_div(char* id, floatpt* fp_p, floatpt fp_a, floatpt fp_b) {
  double numerator = floatpt2num(fp_a);
  double denominator = floatpt2num(fp_b);
  double result = numerator/denominator;
  set_floatpt_value(id, fp_p, result);
}

inline void fl_inv(char* id, floatpt* fp_p, floatpt fp_a) {
  double denominator = floatpt2num(fp_a);
  double result = 1.0/denominator;
  set_floatpt_value(id, fp_p, result);
}

inline void fl_exp(char* id, floatpt* fp_p, floatpt fp_a) {
  double exponent = floatpt2num(fp_a);
  double result = exp(exponent);
  set_floatpt_value(id, fp_p, result);
}

inline void fl_log(char* id, floatpt* fp_p, floatpt fp_a) {
  double exponent = floatpt2num(fp_a);
  double result = log(exponent);
  set_floatpt_value(id, fp_p, result);
}

inline void fl_sin(char* id, floatpt* fp_p, floatpt fp_a) {
  double radians = floatpt2num(fp_a);
  double result = sin(radians);
  set_floatpt_value(id, fp_p, result);
}

inline void fl_cos(char* id, floatpt* fp_p, floatpt fp_a) {
  double radians = floatpt2num(fp_a);
  double result = cos(radians);
  set_floatpt_value(id, fp_p, result);
}

inline void fl_tan(char* id, floatpt* fp_p, floatpt fp_a) {
  double radians = floatpt2num(fp_a);
  double result = tan(radians);
  set_floatpt_value(id, fp_p, result);
}

inline void fl_sinh(char* id, floatpt* fp_p, floatpt fp_a) {
  double radians = floatpt2num(fp_a);
  double result = sinh(radians);
  set_floatpt_value(id, fp_p, result);
}

inline void fl_cosh(char* id, floatpt* fp_p, floatpt fp_a) {
  double radians = floatpt2num(fp_a);
  double result = cosh(radians);
  set_floatpt_value(id, fp_p, result);
}

inline void fl_tanh(char* id, floatpt* fp_p, floatpt fp_a) {
  double radians = floatpt2num(fp_a);
  double result = tanh(radians);
  set_floatpt_value(id, fp_p, result);
}
