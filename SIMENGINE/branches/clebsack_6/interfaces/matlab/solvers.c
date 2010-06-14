#include <stdio.h>
#include <math.h>
#define MIN(a,b) ((a<b)?a:b)
#define MAX(a,b) ((a>b)?a:b)

int forwardeuler(int(*flows)(CDATAFORMAT, const CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, int), CDATAFORMAT *model_states, CDATAFORMAT *time, CDATAFORMAT timestep, CDATAFORMAT *params) {

  static CDATAFORMAT k1[STATESPACE];

  CDATAFORMAT *outputs;

  int ret = (*flows)(*time, model_states, k1, params, outputs, 1);

  int i;
  for(i=STATESPACE-1; i>=0; i--) {
    model_states[i] = model_states[i] + timestep * k1[i];
  }

  *time += timestep;

  return ret;
}

int rk4(int(*flows)(CDATAFORMAT, const CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, int), CDATAFORMAT *model_states, CDATAFORMAT *time, CDATAFORMAT timestep, CDATAFORMAT *params) {

  static CDATAFORMAT k1[STATESPACE];
  static CDATAFORMAT k2[STATESPACE];
  static CDATAFORMAT k3[STATESPACE];
  static CDATAFORMAT k4[STATESPACE];

  static CDATAFORMAT temp[STATESPACE];

  CDATAFORMAT *outputs;
  int i;
  int ret;
  ret = (*flows)(*time, model_states, k1, params, outputs, 1);
  for(i=STATESPACE-1; i>=0; i--) {
    temp[i] = model_states[i] + (timestep/2)*k1[i];
  }
  ret |= (*flows)(*time+(timestep/2), temp, k2, params, outputs, 0);

  for(i=STATESPACE-1; i>=0; i--) {
    temp[i] = model_states[i] + (timestep/2)*k2[i];
  }
  ret |= (*flows)(*time+(timestep/2), temp, k3, params, outputs, 0);

  for(i=STATESPACE-1; i>=0; i--) {
    temp[i] = model_states[i] + timestep*k3[i];
  }
  ret |= (*flows)(*time+timestep, temp, k4, params, outputs, 0);

  for(i=STATESPACE-1; i>=0; i--) {
    model_states[i] = model_states[i] + (timestep/6.0) * (k1[i] + 2*k2[i] + 2*k3[i] + k4[i]);
  }

  *time += timestep;

  return ret;
}

int bogacki_shampine(int(*flows)(CDATAFORMAT, const CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, int), CDATAFORMAT *model_states, CDATAFORMAT *time, CDATAFORMAT timestep, CDATAFORMAT *params) {

  static CDATAFORMAT k1[STATESPACE];
  static CDATAFORMAT k2[STATESPACE];
  static CDATAFORMAT k3[STATESPACE];
  static CDATAFORMAT k4[STATESPACE];
  static CDATAFORMAT temp[STATESPACE];
  static CDATAFORMAT next_states[STATESPACE];
  static CDATAFORMAT z_next_states[STATESPACE];

  CDATAFORMAT *outputs;
  static CDATAFORMAT cur_timestep = 0;
  if (cur_timestep == 0) cur_timestep = timestep;
		      
  CDATAFORMAT max_timestep = timestep*256;
  CDATAFORMAT min_timestep = timestep/256;

  //fprintf(stderr, "ts=%g\n", cur_timestep);

  int i;
  int ret = (*flows)(*time, model_states, k1, params, outputs, 1);

  int appropriate_step = FALSE;

  CDATAFORMAT max_error;

  while(!appropriate_step) {

    //fprintf(stderr, "|-> ts=%g", cur_timestep);
    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (cur_timestep/2)*k1[i];
    }
    ret |= (*flows)(*time+(cur_timestep/2), temp, k2, params, outputs, 0);

    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (3*cur_timestep/4)*k2[i];
    }
    ret |= (*flows)(*time+(3*cur_timestep/4), temp, k3, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      next_states[i] = model_states[i] + (2.0/9.0)*cur_timestep*k1[i] + (1.0/3.0)*cur_timestep*k2[i] + (4.0/9.0)*cur_timestep*k3[i];
    }
    
    // now compute k4 to adapt the step size
    ret |= (*flows)(*time+cur_timestep, next_states, k4, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      z_next_states[i] = model_states[i] + (7.0/24.0)*cur_timestep*k1[i] + 0.25*cur_timestep*k2[i] + (1.0/3.0)*cur_timestep*k3[i] + 0.125*cur_timestep*k4[i];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = -1e20;
    CDATAFORMAT max_allowed_error;
    CDATAFORMAT err_sum = 0;
    CDATAFORMAT next_timestep;

    int acceptable_error = TRUE;
    for(i=STATESPACE-1; i>=0; i--) {
      err = fabs(next_states[i]-z_next_states[i]);
      max_allowed_error = RELTOL*fabs(next_states[i])+ABSTOL;
      if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
      
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio>max_error ? ratio : max_error;
      err_sum += ratio*ratio;
      //mexPrintf("%g (%g-%g) ", ratio, next_states[i], z_next_states[i]);

    }
    
    CDATAFORMAT norm = max_error;
    //CDATAFORMAT norm = sqrt(err_sum/STATESPACE);
    appropriate_step = norm <= 1;

    if (appropriate_step)
      *time += cur_timestep;

    next_timestep = 0.90 * cur_timestep*pow(1.0/norm, 1.0/3.0);
    //mexPrintf("ts: %g -> %g (norm=%g)\n", cur_timestep, next_timestep, norm);

    cur_timestep = next_timestep;

  }

  // just return back the expected
  for(i=STATESPACE-1; i>=0; i--) {
    model_states[i] = next_states[i];
  }

  return ret;
}

int bogacki_shampine_save(int(*flows)(CDATAFORMAT, const CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, int), CDATAFORMAT *model_states, CDATAFORMAT *time, CDATAFORMAT timestep, CDATAFORMAT *params) {

  static CDATAFORMAT k1[STATESPACE];
  static CDATAFORMAT k2[STATESPACE];
  static CDATAFORMAT k3[STATESPACE];
  static CDATAFORMAT k4[STATESPACE];
  static CDATAFORMAT temp[STATESPACE];
  static CDATAFORMAT next_states[STATESPACE];
  static CDATAFORMAT z_next_states[STATESPACE];

  CDATAFORMAT *outputs;
  static CDATAFORMAT cur_timestep = 0;
  if (cur_timestep == 0) cur_timestep = timestep;
		      
  CDATAFORMAT max_timestep = timestep*256;
  CDATAFORMAT min_timestep = timestep/256;

  //fprintf(stderr, "ts=%g\n", cur_timestep);

  int i;
  int ret = (*flows)(*time, model_states, k1, params, outputs, 1);

  int appropriate_step = FALSE;

  CDATAFORMAT max_error;

  while(!appropriate_step) {

    //fprintf(stderr, "|-> ts=%g", cur_timestep);
    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (cur_timestep/2)*k1[i];
    }
    ret |= (*flows)(*time+(cur_timestep/2), temp, k2, params, outputs, 0);

    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (3*cur_timestep/4)*k2[i];
    }
    ret |= (*flows)(*time+(3*cur_timestep/4), temp, k3, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      next_states[i] = model_states[i] + (2.0/9.0)*cur_timestep*k1[i] + (1.0/3.0)*cur_timestep*k2[i] + (4.0/9.0)*cur_timestep*k3[i];
    }
    
    // now compute k4 to adapt the step size
    ret |= (*flows)(*time+cur_timestep, next_states, k4, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      z_next_states[i] = model_states[i] + (7.0/24.0)*cur_timestep*k1[i] + 0.25*cur_timestep*k2[i] + (1.0/3.0)*cur_timestep*k3[i] + 0.125*cur_timestep*k4[i];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = -1e20;
    CDATAFORMAT max_allowed_error;
    int acceptable_error = TRUE;
    for(i=STATESPACE-1; i>=0; i--) {
      //fprintf(stderr,"next, z: (%g, %g)\n", next_states[i], z_next_states[i]);
      err = fabs(next_states[i]-z_next_states[i]);
      max_allowed_error = RELTOL*fabs(next_states[i])+ABSTOL;
      if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
      //printf("Error[%d] = %g\n", i, err);
      
      if ((max_error > ABSTOL) && (cur_timestep > min_timestep)) {
	acceptable_error = FALSE;
	//cur_timestep /= 1.5;
      }
      /*else if ((err < ABSTOL/2) && (cur_timestep < max_timestep)) {
	//cur_timestep *= 1.5; // for the next iteration
	acceptable_error &= TRUE;
	}*/
      else {
	acceptable_error &= TRUE;
      }
      appropriate_step = acceptable_error;
    }
    
    if (appropriate_step)
      *time += cur_timestep;

    // update the timestep for the next iteration
    //if (max_error == 0) max_error=ABSTOL/4;
    //fprintf(stderr, "(error=%g), previous_timestep=%g, ratio=%g, ", max_error, cur_timestep, (ABSTOL/max_error));
    //cur_timestep = cur_timestep - 0.9*cur_timestep * (1-ABSTOL/max_error);
    //fprintf(stderr, " max_error=%g\n", max_error);
    if (-max_error > cur_timestep*2) // the error is too low, increase the time step
      cur_timestep *= 2;
    else if (max_error > cur_timestep/2) // the error is too high, halve the time step
      cur_timestep /= 2;
    else
      cur_timestep = cur_timestep - max_error;

    //fprintf(stderr, "cur_timestep=%g\n", cur_timestep);
    if (cur_timestep < min_timestep) {
      cur_timestep = min_timestep;
    }
    else if (cur_timestep > max_timestep) {
      cur_timestep = max_timestep;
    }
    
  }

  // just return back the expected
  for(i=STATESPACE-1; i>=0; i--) {
    model_states[i] = next_states[i];
  }

  return ret;
}

int dormand_prince(int(*flows)(CDATAFORMAT, const CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, int), CDATAFORMAT *model_states, CDATAFORMAT *time, CDATAFORMAT timestep, CDATAFORMAT *params) {

  static CDATAFORMAT k1[STATESPACE];
  static CDATAFORMAT k2[STATESPACE];
  static CDATAFORMAT k3[STATESPACE];
  static CDATAFORMAT k4[STATESPACE];
  static CDATAFORMAT k5[STATESPACE];
  static CDATAFORMAT k6[STATESPACE];
  static CDATAFORMAT k7[STATESPACE];
  static CDATAFORMAT temp[STATESPACE];
  static CDATAFORMAT next_states[STATESPACE];
  static CDATAFORMAT z_next_states[STATESPACE];

  CDATAFORMAT *outputs;
  static CDATAFORMAT cur_timestep = 0;
  if (cur_timestep == 0) cur_timestep = timestep;
		      
  CDATAFORMAT max_timestep = timestep*256;
  CDATAFORMAT min_timestep = timestep/256;

  //fprintf(stderr, "ts=%g\n", cur_timestep);

  int i;
  int ret = (*flows)(*time, model_states, k1, params, outputs, 1);

  int appropriate_step = FALSE;

  CDATAFORMAT max_error;

  while(!appropriate_step) {

    //fprintf(stderr, "|-> ts=%g", cur_timestep);
    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (cur_timestep/5.0)*k1[i];
    }
    ret |= (*flows)(*time+(cur_timestep/5.0), temp, k2, params, outputs, 0);

    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (3.0*cur_timestep/40.0)*k1[i] + (9.0*cur_timestep/4.0)*k2[i];
    }
    ret |= (*flows)(*time+(3.0*cur_timestep/10.0), temp, k3, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (44.0*cur_timestep/45.0)*k1[i] + (-56.0*cur_timestep/15.0)*k2[i] + (32.0*cur_timestep/9.0)*k3[i];
    }
    ret |= (*flows)(*time+(4.0*cur_timestep/5.0), temp, k4, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (19372.0*cur_timestep/6561.0)*k1[i] + (-25360.0*cur_timestep/2187.0)*k2[i] + (64448.0*cur_timestep/6561.0)*k3[i] + (-212.0*cur_timestep/729.0)*k4[i];
    }
    ret |= (*flows)(*time+(8.0*cur_timestep/9.0), temp, k5, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = model_states[i] + (9017.0*cur_timestep/3168.0)*k1[i] + (-355.0*cur_timestep/33.0)*k2[i] + (46732.0*cur_timestep/5247.0)*k3[i] + (49.0*cur_timestep/176.0)*k4[i] + (-5103.0*cur_timestep/18656.0)*k5[i];
    }
    ret |= (*flows)(*time+cur_timestep, temp, k6, params, outputs, 0);
    
    for(i=STATESPACE-1; i>=0; i--) {
      next_states[i] = model_states[i] + (35.0*cur_timestep/384.0)*k1[i] + (500.0*cur_timestep/1113.0)*k3[i] + (125.0*cur_timestep/192.0)*k4[i] + (-2187.0*cur_timestep/6784.0)*k5[i] + (11.0*cur_timestep/84.0)*k6[i];
    }
    
    // now compute k4 to adapt the step size
    ret |= (*flows)(*time+cur_timestep, next_states, k7, params, outputs, 0);
    
    CDATAFORMAT E1 = 71.0/57600.0;
    CDATAFORMAT E3 = -71.0/16695.0;
    CDATAFORMAT E4 = 71.0/1920.0;
    CDATAFORMAT E5 = -17253.0/339200.0;
    CDATAFORMAT E6 = 22.0/525.0;
    CDATAFORMAT E7 = -1.0/40.0;
    for(i=STATESPACE-1; i>=0; i--) {
      temp[i] = /*next_states[i] + */cur_timestep*(E1*k1[i] + E3*k3[i] + E4*k4[i] + E5*k5[i] + E6*k6[i] + E7*k7[i]);
      //z_next_states[i] = model_states[i] + (71*cur_timestep/57600)*k1[i] + (-71*cur_timestep/16695)*k3[i] + (71*cur_timestep/1920)*k4[i] + (-17253*cur_timestep/339200)*k5[i] + (22*cur_timestep/525)*k6[i] + (-1*cur_timestep/40)*k7[i];
      //z_next_states[i] = model_states[i] + (5179*cur_timestep/57600)*k1[i] + (7571*cur_timestep/16695)*k3[i] + (393*cur_timestep/640)*k4[i] + (-92097*cur_timestep/339200)*k5[i] + (187*cur_timestep/2100)*k6[i] + (1*cur_timestep/40)*k7[i];
    }

    // compare the difference
    CDATAFORMAT err;
    max_error = -1e20;
    CDATAFORMAT max_allowed_error;
    CDATAFORMAT err_sum = 0.0;
    CDATAFORMAT next_timestep;

    int acceptable_error = TRUE;
    for(i=STATESPACE-1; i>=0; i--) {
      err = temp[i];
      max_allowed_error = RELTOL*MAX(fabs(next_states[i]),fabs(model_states[i]))+ABSTOL;


      //err = fabs(next_states[i]-z_next_states[i]);
      //max_allowed_error = RELTOL*fabs(next_states[i])+ABSTOL;
      //if (err-max_allowed_error > max_error) max_error = err - max_allowed_error;
			       
      CDATAFORMAT ratio = (err/max_allowed_error);
      max_error = ratio>max_error ? ratio : max_error;
      err_sum += ratio*ratio;

      //mexPrintf("%g (%g-%g) ", ratio, next_states[i], z_next_states[i]);
    }
    
    //CDATAFORMAT norm = max_error; 
    CDATAFORMAT norm = sqrt(err_sum/((CDATAFORMAT)STATESPACE));
    appropriate_step = norm <= 1;

    if (appropriate_step)
      *time += cur_timestep;

    next_timestep = 0.9 * cur_timestep*pow(1.0/norm, 1.0/5.0);
    //mexPrintf("ts: %g -> %g (norm=%g)\n", cur_timestep, next_timestep, norm);

    cur_timestep = next_timestep;
    
  }

  // just return back the expected
  for(i=STATESPACE-1; i>=0; i--) {
    model_states[i] = next_states[i];
  }

  return ret;
}

