#include <stdio.h>
#include <math.h>

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
		      
  CDATAFORMAT max_timestep = timestep*8;
  CDATAFORMAT min_timestep = timestep/8;

  int i;
  int ret = (*flows)(*time, model_states, k1, params, outputs, 1);

  int appropriate_step = FALSE;

  CDATAFORMAT max_error;

  while(!appropriate_step) {

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
    max_error = 0;
    int acceptable_error = TRUE;
    for(i=STATESPACE-1; i>=0; i--) {
      //printf("next, z: (%g, %g)\n", next_states[i], z_next_states[i]);
      err = fabs(next_states[i]-z_next_states[i]);
      if (err > max_error) max_error = err;
      //printf("Error[%d] = %g\n", i, err);
      
      if ((err > ABSTOL) && (cur_timestep > min_timestep)) {
	acceptable_error = FALSE;
	//cur_timestep /= 1.5;
      }
      else if ((err < ABSTOL/2) && (cur_timestep < max_timestep)) {
	//cur_timestep *= 1.5; // for the next iteration
	acceptable_error &= TRUE;
      }
      else {
	acceptable_error &= TRUE;
      }
      appropriate_step = acceptable_error;
    }
    
    if (appropriate_step)
      *time += cur_timestep;

    // update the timestep for the next iteration
    if (max_error == 0) max_error=ABSTOL/4;
    //fprintf(stderr, "(error=%g), previous_timestep=%g, ratio=%g, ", max_error, cur_timestep, (ABSTOL/max_error));
    cur_timestep = cur_timestep - 0.9*cur_timestep * (1-ABSTOL/max_error);
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

