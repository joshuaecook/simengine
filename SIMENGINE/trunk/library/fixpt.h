// Dynamo Fixed Point Operation Definitions
// Copyright 2007 Simatra Modeling Technologies
#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>
#include <math.h>
#include <float.h>

typedef struct 
{
  int sign; 
  int bits; 
  int frac;
  mpz_t* v;
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
} fixpt;

void fixpt2prec(char* str, fixpt fp) {
  char* s = (fp.sign) ? "" : "U";
  sprintf(str, "%sFix%d_%d", s, fp.bits, fp.frac);
}

double fixpt2num(fixpt fp) {
  int sign = fp.sign;
  int bits = fp.bits;
  int frac = fp.frac;
  mpz_t* v = (mpz_t *)fp.v;

  double val = mpz_get_d(*v) / pow(2,frac);
  return val;
}

// Constructor
inline fixpt create_fixpt(int sign, int bits, int frac, mpz_t* v_ptr) {
  fixpt temp;
  temp.sign = sign;
  temp.bits = bits;
  temp.frac = frac;
  temp.v = v_ptr;
  temp.max = 0;
  temp.min = 0;
  double num = fixpt2num(temp);
  temp.max = num;
  temp.min = num;
  temp.count = 0;
  temp.underflows = 0;
  temp.overflows = 0;
  temp.negsats = 0;
  temp.prev = 0;
  temp.same = 0;
  temp.min_dx = DBL_MAX;
  temp.max_dx = 0;
  return temp;
} 

// Initialize a fixpt quantity without defining a value
fixpt init_fixpt (int sign, int bits, int frac) {
  mpz_t* v = (mpz_t *)malloc(sizeof(mpz_t));
  mpz_init2(*v, bits);
  mpz_set_d(*v, 0);
  fixpt fp = create_fixpt(sign, bits, frac, v);
  fp.min = DBL_MAX;
  fp.max = -DBL_MAX;//DBL_MIN;

  return fp;
}

// Generates a fixpt number and initializes it with a value
fixpt num2fixpt (double value, int sign, int bits, int frac) {
  fixpt fp = init_fixpt(sign, bits, frac);

  mpz_t* v = fp.v;

  mpz_set_d(*v,value * pow(2,frac));
  fp.max = value;
  fp.min = value;
  fp.count = 1;

  return fp;
}

void fixpt2str (char* str, fixpt fp) {
  double val = fixpt2num(fp);

  sprintf(str,"%10g",val);
}

void fixpt2stats (char* str, fixpt fp) {
  mpz_t* v = fp.v;
  char prec[20];
  fixpt2prec(prec, fp);
  char val[50];
  fixpt2str(val, fp);
  if (fp.count == fp.same + 1) {
    gmp_sprintf(str,"Internal=%Zd, Real=%s, Precision=%s, Count=%d, Max=%.13g, Min=%.13g, Overflows=%d, Underflows=%d, NegSats=%d, Same=%d, MaxDx=N/A, MinDx=N/A", *v, val, prec, fp.count, fp.max, fp.min, fp.overflows, fp.underflows, fp.negsats, /*fp.prev,*/ fp.same);
  }
  else {
    gmp_sprintf(str,"Internal=%Zd, Real=%s, Precision=%s, Count=%d, Max=%.13g, Min=%.13g, Overflows=%d, Underflows=%d, NegSats=%d, Same=%d, MaxDx=%.13g, MinDx=%.13g", *v, val, prec, fp.count, fp.max, fp.min, fp.overflows, fp.underflows, fp.negsats, /*fp.prev,*/ fp.same, fp.max_dx, fp.min_dx);
  }

}

void print_fp (fixpt fp) {
  char str[250];
  fixpt2stats(str, fp);
  fprintf(stderr,"%s\n", str);
  fflush(stderr);
}


inline void conv_fixpt(char* id, fixpt* fp_ptr, int sign, int bits, int frac) {
  
  fixpt fp = *fp_ptr;
  
  int s = fp.sign;
  int b = fp.bits;
  int f = fp.frac;
  mpz_t* v_ptr = fp.v;

  size_t fp_sign = mpz_sgn(*v_ptr);
  int isNeg = fp_sign == -1;
  int isZero = fp_sign == 0;

  // first check the sign...
  if (isNeg && sign == 0) { // if the output is unsigned, but the value is negative
    //fprintf(stderr,"[%s] Saturating negative value to zero -> ", id);
    //print_fp(fp);
    fp.negsats++;
    // removing the automatic zero'ing when a negative saturation occurs
    //mpz_set_d(*v_ptr,0);
    //    return;
  }

  // add or remove fractional bits
  if (frac > f) { // if need to add fractional bits
    mpz_mul_2exp(*v_ptr, *v_ptr, frac-f);
  }
  else if (frac < f) {
    //fprintf(stderr, "conv_fixpt (%d): ", mpz_tstbit(*v_ptr, 0)); print_fp(fp);
    // The mpz_?div_q_2exp performs the rounding.  tdiv is truncate, fdiv rounds down, and cdiv rounds up
    // The hardware rounds down based on its 2's complement notation
    //    mpz_tdiv_q_2exp(*v_ptr, *v_ptr, f-frac);
    mpz_fdiv_q_2exp(*v_ptr, *v_ptr, f-frac);
  }

  // write back the precision info
  fp.sign = sign;
  fp.bits = bits;
  fp.frac = frac;

  // update min and max
  double num = fixpt2num(fp);
  if (num > fp.max) {
    fp.max = num;
  }
  if (num < fp.min) {
    fp.min = num;
  }

  // check for overflow
  if (mpz_sizeinbase(*v_ptr, 2) > (bits-sign)) {
    //fprintf(stderr,"[%s] Overflow detected -> ", id);
    //print_fp(fp);
    fp.overflows++;
  } 

  // check for underflow
  if (!isZero && mpz_sgn(*v_ptr)==0) { // if fp wasn't zero but became zero, then it is an underflow
    //fprintf(stderr,"[%s] Underflow detected -> ", id);
    //print_fp(fp);
    fp.underflows++;
  }

  *fp_ptr = fp;

}

// realign_fixpt - updates internal value to match desired precision
// fp - has incorrect precision, the mpz term has the specified precision
void realign_fixpt(char* id, fixpt* fp, int sign, int bits, int frac) {
  fixpt temp = create_fixpt(sign, bits, frac, (*fp).v);

  // copy over min/max
  temp.min = (*fp).min;
  temp.max = (*fp).max;
  double prev_min, prev_max;
  prev_min = temp.min;
  prev_max = temp.max;

  conv_fixpt(id, &temp, (*fp).sign, (*fp).bits, (*fp).frac);

  // copy back values
  (*fp).v = temp.v;
  (*fp).min = temp.min;
  (*fp).max = temp.max;
  (*fp).overflows += temp.overflows;
  (*fp).underflows += temp.underflows;
  (*fp).negsats += temp.negsats;
}

inline void update_value(fixpt* fp, double prev) {

  // store the previous value (if at least one value has been stored)
  if (fp->count > 0) {
    fp->prev = prev;
  }

  // retrieve the new value
  double new = fixpt2num(*fp);

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

void set_fixpt(char* id, fixpt* fp, double value, int sign, int bits, int frac) {
  mpz_t* v_ptr = (*fp).v;
  mpz_set_d(*v_ptr, value * pow(2,frac));

  // set required bits to handle the passed value
  (*fp).sign = (value < 0);
  (*fp).bits = mpz_sizeinbase(*v_ptr, 2) + (value < 0);
  (*fp).frac = frac;

  // adjust the fixed point to the appropriate precision
  conv_fixpt(id, fp, sign, bits, frac);
}

inline void set_fixpt_value(char* id, fixpt* fp, double value) {
  // save the current precision
  int sign = (*fp).sign;
  int bits = (*fp).bits;
  int frac = (*fp).frac;

  mpz_t* v_ptr = (*fp).v;

  // store the previous value
  double prev = fixpt2num(*fp);

  // set the new value
  // the scaled test is performed to fix a bug present on Intel Macs.  This problem may exist elsewhere.
  // Basically, values under 2^-32 cause a seg fault
  double scaled = value*pow(2,frac);
  double MIN_VALUE = pow(2,-32);
#ifdef darwin
  if (scaled > 0 && scaled < MIN_VALUE) {
    mpz_set_d(*v_ptr, 0);
  }
  else {
#endif
    // default case
    // for ticket #433, found that the hardware was rounding any constant like this, so we should too
    if (scaled >= 0) {
      mpz_set_d(*v_ptr, scaled + 0.5);
    }
    else {
      mpz_set_d(*v_ptr, scaled - 0.5);
    }
    //mpz_set_d(*v_ptr, scaled);
#ifdef darwin
  }
#endif

  //char str[250];
  //gmp_sprintf(str, "%Zd", *v_ptr);
  //fprintf(stderr, "value * 2^%d = scaled (%f -> %f) Internal=%s\n", frac, value, scaled, str);

  // set required bits to handle the passed value
  (*fp).sign = (value < 0);
  (*fp).bits = mpz_sizeinbase(*v_ptr, 2) + (value < 0);
  (*fp).frac = frac;

  // adjust the fixed point back to the appropriate precision
  conv_fixpt(id, fp, sign, bits, frac);

  // update statistics
  update_value(fp, prev);
}

inline void asgn_fixpt(fixpt *target, fixpt *source) {

  target->sign = source->sign;
  target->bits = source->bits;
  target->frac = source->frac;
  target->min = source->min;
  target->max = source->max;
  target->overflows = source->overflows;
  target->underflows = source->underflows;
  target->negsats = source->negsats;
  target->prev = source->prev;

  mpz_set(*(target->v), *(source->v));
}

inline void asgn_fixpt_value(char* id, fixpt *target, fixpt *source) {

  // store the previous precision
  int sign = target->sign;
  int bits = target->bits;
  int frac = target->frac;

  // store the previous value
  double prev = fixpt2num(*target);
  //double overflows = target->overflows;
  //double underflows = target->underflows;
  //double negsats = target->overflows;

  // copy over the new precision
  asgn_fixpt (target, source);
  
  // run conv_fixpt to convert to the desired precision
  conv_fixpt (id, target, sign, bits, frac);

  // update statistics
  update_value(target, prev);

  // add back the overflows, etc.
  //target->overflows += overflows;
  //target->underflows += underflows;
  //target->negsats += negsats;

}

inline fixpt copy_fp(fixpt fp) {
  mpz_t* v_cp=(mpz_t *)malloc(sizeof(mpz_t));
  mpz_init_set(*v_cp, *fp.v);
  
  fixpt fp_cp = create_fixpt(fp.sign, fp.bits, fp.frac, v_cp);

  asgn_fixpt(&fp_cp, &fp);

  return fp_cp;
}

inline floatpt num2floatpt(double value);

// Conversion functions
inline floatpt fixpt2floatpt(fixpt fp) {
  double val = fixpt2num(fp);
  return num2floatpt(val);
}


inline void fp_mult(char* id, fixpt* fp_p, fixpt fp_a, fixpt fp_b) {
  int s_p = (*fp_p).sign;
  int b_p = (*fp_p).bits;
  int f_p = (*fp_p).frac;
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t* v_ptr_p = (*fp_p).v;
  mpz_t* v_ptr_a = fp_a.v;
  mpz_t* v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*fp_p);

  mpz_mul(*v_ptr_p, *v_ptr_a, *v_ptr_b);

  realign_fixpt(id, fp_p, s_a | s_b, (b_a-s_a) + (b_b-s_b) + (s_a | s_b), f_a + f_b);

  // update statistics
  update_value(fp_p, prev);
}

inline void fp_mul (char* id, fixpt* fp_p, fixpt fp_a, fixpt fp_b) {
  fp_mult(id, fp_p, fp_a, fp_b);
}

inline int max(int a, int b) {
  if (a > b)
    return a;
  else
    return b;
}

inline void fp_cast(char* id, fixpt* fp_p, fixpt fp_a) {

  //fprintf(stderr, "fp_a = "); print_fp(fp_a);
  //double val_a = fixpt2num(fp_a);
  //  fprintf(stderr, "val_a = %f\n", val_a);
  //return set_fixpt_value(id, fp_p, val_a);
  asgn_fixpt_value(id, fp_p, &fp_a);
  
}

inline void fp_cast_fl(char* id, fixpt* fp_p, floatpt fl_a) {

  //fprintf(stderr, "fp_a = "); print_fp(fp_a);
  //double val_a = fixpt2num(fp_a);
  //  fprintf(stderr, "val_a = %f\n", val_a);
  //return set_fixpt_value(id, fp_p, val_a);
  set_fixpt_value(id, fp_p, floatpt2num(fl_a));
  
}

inline void fl_cast_fp(char* id, floatpt* fl_p, fixpt fp_a) {

  set_floatpt_value(id, fl_p, fixpt2num(fp_a));
  
}


inline void fp_add(char* id, fixpt* fp_p, fixpt fp_a, fixpt fp_b) {
  int s_p = (*fp_p).sign;
  int b_p = (*fp_p).bits;
  int f_p = (*fp_p).frac;
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_p = (*fp_p).v;
  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*fp_p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the addition
  mpz_add(*v_ptr_p, *temp_a.v, *temp_b.v);

  realign_fixpt(id, fp_p, s_a | s_b, max(b_a-s_a,b_b-b_b) + (s_a | s_b) + 1, max(f_a,f_b));

  // update statistics
  update_value(fp_p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}


inline void fp_sub(char* id, fixpt* fp_p, fixpt fp_a, fixpt fp_b) {
  int s_p = (*fp_p).sign;
  int b_p = (*fp_p).bits;
  int f_p = (*fp_p).frac;
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_p = (*fp_p).v;
  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*fp_p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the subtraction
  mpz_sub(*v_ptr_p, *temp_a.v, *temp_b.v);

  realign_fixpt(id, fp_p, s_a | s_b, max(b_a-s_a,b_b-b_b) + (s_a | s_b) + 1, max(f_a,f_b));

  // update statistics
  update_value(fp_p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}

inline void fp_neg(char* id, fixpt* fp_p, fixpt fp_a) {
  fp_sub(id, fp_p, num2fixpt(0.0, 0, 1, 0), fp_a);
}

inline void fp_neq(char* id, fixpt* p, fixpt fp_a, fixpt fp_b) {
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the comparison
  int cmp = mpz_cmp(*temp_a.v, *temp_b.v);

  set_fixpt_value(id, p,cmp != 0);

  // update statistics
  update_value(p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}

inline void fp_equal(char* id, fixpt* p, fixpt fp_a, fixpt fp_b) {
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the comparison
  int cmp = mpz_cmp(*temp_a.v, *temp_b.v);

  set_fixpt_value(id, p,cmp == 0);

  // update statistics
  update_value(p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}

inline void fp_gt(char* id, fixpt* p, fixpt fp_a, fixpt fp_b) {
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the comparison
  int cmp = mpz_cmp(*temp_a.v, *temp_b.v);

  set_fixpt_value(id, p,cmp > 0);

  // update statistics
  update_value(p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}

inline void fp_lt(char* id, fixpt* p, fixpt fp_a, fixpt fp_b) {
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the comparison
  int cmp = mpz_cmp(*temp_a.v, *temp_b.v);

  set_fixpt_value(id, p,cmp < 0);

  // update statistics
  update_value(p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}

inline void fp_ge(char* id, fixpt* p, fixpt fp_a, fixpt fp_b) {
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the comparison
  int cmp = mpz_cmp(*temp_a.v, *temp_b.v);

  set_fixpt_value(id, p,cmp >= 0);

  // update statistics
  update_value(p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}

inline void fp_le(char* id, fixpt* p, fixpt fp_a, fixpt fp_b) {
  int s_a = fp_a.sign;
  int b_a = fp_a.bits;
  int f_a = fp_a.frac;
  int s_b = fp_b.sign;
  int b_b = fp_b.bits;
  int f_b = fp_b.frac;

  mpz_t *v_ptr_a = fp_a.v;
  mpz_t *v_ptr_b = fp_b.v;

  // store the previous value
  double prev = fixpt2num(*p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_a);
  fixpt temp_b = copy_fp(fp_b);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  // perform the comparison
  int cmp = mpz_cmp(*temp_a.v, *temp_b.v);

  set_fixpt_value(id, p,cmp <= 0);

  // update statistics
  update_value(p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);
}


inline void fp_if(char* id, fixpt* fp_p, fixpt fp_cond, fixpt fp_ift, fixpt fp_iff) {
  int s_p = (*fp_p).sign;
  int b_p = (*fp_p).bits;
  int f_p = (*fp_p).frac;
  int s_a = fp_ift.sign;
  int b_a = fp_ift.bits;
  int f_a = fp_ift.frac;
  int s_b = fp_iff.sign;
  int b_b = fp_iff.bits;
  int f_b = fp_iff.frac;

  mpz_t* v_ptr_p = (*fp_p).v;
  
  // store the previous value
  double prev = fixpt2num(*fp_p);

  // copy the fixpt format
  fixpt temp_a = copy_fp(fp_ift);
  fixpt temp_b = copy_fp(fp_iff);

  // aligned the operand with fewer fractional bits to the other operand
  if (f_a > f_b) {
    conv_fixpt(id, &temp_b,s_b,b_b+(f_a-f_b),f_a);
  }
  else if (f_b > f_a) {
    conv_fixpt(id, &temp_a,s_a,b_a+(f_b-f_a),f_b);
  }

  if (fixpt2num(fp_cond)) {
    mpz_set(*v_ptr_p, *temp_a.v);
  }
  else {
    mpz_set(*v_ptr_p, *temp_b.v);
  }

  realign_fixpt(id, fp_p, s_a | s_b, max(b_a-s_a,b_b-b_b) + (s_a | s_b) + 1, max(f_a,f_b));

  // create temp_fp to have the appropriate precision
  //fixpt temp_fp = create_fixpt(s_a | s_b, max(b_a-s_a,b_b-b_b) + (s_a | s_b) + 1, max(f_a,f_b), v_ptr_p);

  // convert to appropriate precision
  //conv_fixpt(id, &temp_fp, s_p, b_p, f_p);

  // write back the new fixed point value
  //(*fp_p).v = temp_fp.v;

  // update statistics
  update_value(fp_p, prev);

  // clear temporary variables
  mpz_clear(*temp_a.v);
  mpz_clear(*temp_b.v);
  free(temp_a.v);
  free(temp_b.v);

}

inline void fp_not(char* id, fixpt* fp_p, fixpt fp_a) {
  fp_if(id, fp_p, fp_a, num2fixpt(0.0, 0, 1, 0), num2fixpt(1.0, 0, 1, 0));
}

inline void fp_or(char* id, fixpt* fp_p, fixpt fp_a, fixpt fp_b) {
  fp_if(id, fp_p, fp_a, fp_a, fp_b);
}

inline void fp_and(char* id, fixpt* fp_p, fixpt fp_a, fixpt fp_b) {
  fp_if(id, fp_p, fp_a, fp_b, num2fixpt(0.0, 0, 1, 0));
}

// Operations that are not normally supported in hardware will use double approximations
inline void fp_div(char* id, fixpt* fp_p, fixpt fp_a, fixpt fp_b) {
  double numerator = fixpt2num(fp_a);
  double denominator = fixpt2num(fp_b);
  double result = numerator/denominator;
  set_fixpt_value(id, fp_p, result);
}

inline void fp_inv(char* id, fixpt* fp_p, fixpt fp_a) {
  double denominator = fixpt2num(fp_a);
  double result = 1.0/denominator;
  set_fixpt_value(id, fp_p, result);
}

inline void fp_exp(char* id, fixpt* fp_p, fixpt fp_a) {
  double exponent = fixpt2num(fp_a);
  double result = exp(exponent);
  set_fixpt_value(id, fp_p, result);
}

inline void fp_log(char* id, fixpt* fp_p, fixpt fp_a) {
  double exponent = fixpt2num(fp_a);
  double result = log(exponent);
  set_fixpt_value(id, fp_p, result);
}

inline void fp_sin(char* id, fixpt* fp_p, fixpt fp_a) {
  double radians = fixpt2num(fp_a);
  double result = sin(radians);
  set_fixpt_value(id, fp_p, result);
}

inline void fp_cos(char* id, fixpt* fp_p, fixpt fp_a) {
  double radians = fixpt2num(fp_a);
  double result = cos(radians);
  set_fixpt_value(id, fp_p, result);
}

inline void fp_tan(char* id, fixpt* fp_p, fixpt fp_a) {
  double radians = fixpt2num(fp_a);
  double result = tan(radians);
  set_fixpt_value(id, fp_p, result);
}

inline void fp_sinh(char* id, fixpt* fp_p, fixpt fp_a) {
  double radians = fixpt2num(fp_a);
  double result = sinh(radians);
  set_fixpt_value(id, fp_p, result);
}

inline void fp_cosh(char* id, fixpt* fp_p, fixpt fp_a) {
  double radians = fixpt2num(fp_a);
  double result = cosh(radians);
  set_fixpt_value(id, fp_p, result);
}

inline void fp_tanh(char* id, fixpt* fp_p, fixpt fp_a) {
  double radians = fixpt2num(fp_a);
  double result = tanh(radians);
  set_fixpt_value(id, fp_p, result);
}
