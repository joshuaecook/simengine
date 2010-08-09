  /* ********************************************************************************************** */
  // slow low voltage activated K Current Kv4_slow
model (I) = chan_Kv4_s (V, G_Kv4_s, E_K)
  
  //constant E_K = 71 //mV
  
  // fast activation gate "n"
  /*
  PARAMETER MIN_ninf (-0.5 TO 0.5 BY 0.01) = 0 // unitless
  PARAMETER Vh_ninf (-0.040 TO -0.020 BY 0.001) = -0.049 //V // GENESIS equations use real Vhalf
  PARAMETER k_ninf (0.005 TO 0.015 BY 0.0005) = 0.0125 //V
  */
  
  constant MIN_ninf = 0 // unitless
  constant Vh_ninf = -0.049 //V // GENESIS equations use real Vhalf
  constant k_ninf = 0.0125 //V
  
  /*
  PARAMETER tau_n_min (0.0001 TO 0.0005 BY 0.00001) = 0.00025 //s
  PARAMETER tau_n_max (0.005 TO 0.015 BY 0.0005) = 0.007 //s
  PARAMETER Vh_tau_n (-0.040 TO -0.020 BY 0.001) = -0.049 //V // GENESIS equations use real Vhalf
  PARAMETER k_tau1_n (0.020 TO 0.040 BY 0.001) = 0.029 //V
  PARAMETER k_tau2_n (-0.040 TO -0.020 BY 0.001) = -0.029 //V
  */
  
  constant tau_n_min = 0.00025 //s
  constant tau_n_max = 0.007 //s
  constant Vh_tau_n = -0.049 //V // GENESIS equations use real Vhalf
  constant k_tau1_n = 0.029 //V
  constant k_tau2_n = -0.029 //V
  
  equation ninf = MIN_ninf + ((1-MIN_ninf)/(1 + exp ((Vh_ninf - V)/k_ninf)))
  equation tau_n = tau_n_min +((tau_n_max - tau_n_min)/(exp ((Vh_tau_n - V)/k_tau1_n) + exp ((Vh_tau_n-V)/k_tau2_n)))
  
  // fast inactivation gate "h"
  /*
  PARAMETER MIN_hinf (-0.5 TO 0.5 BY 0.01) = 0 // unitless
  PARAMETER Vh_hinf (-0.1 TO -0.060 BY 0.0005) = -0.083 //V
  PARAMETER k_hinf (-0.015 TO -0.005 BY 0.0005) = -0.010 //V
  */
  
  constant MIN_hinf = 0 // unitless
  constant Vh_hinf = -0.083 //V
  constant k_hinf = -0.010 //V
  
  // only the inactivation time constants differ between the Kv4_f and Kv4_s
  /*
  PARAMETER tau_h_min (0.020 TO 0.060 BY 0.0005) = 0.050 //s
  PARAMETER tau_h_max (0.080 TO 0.140 BY 0.0005) = 0.121 //s
  PARAMETER Vh_tau_h (-0.1 TO -0.060 BY 0.0005) = -0.083 //V
  PARAMETER k_tau1_h (0.001 TO 0.020 BY 0.0005) = 0.010 //V
  PARAMETER k_tau2_h (-0.020 TO -0.001 BY 0.0005) = -0.010 //V
  */
  
  constant tau_h_min = 0.050 //s
  constant tau_h_max = 0.121 //s
  constant Vh_tau_h = -0.083 //V
  constant k_tau1_h = 0.010 //V
  constant k_tau2_h = -0.010 //V
  
  equation hinf = MIN_hinf + ((1-MIN_hinf)/(1 + exp ((Vh_hinf - V)/k_hinf)))
  equation tau_h = tau_h_min + ((tau_h_max - tau_h_min)/(exp ((Vh_tau_h - V)/k_tau1_h) + exp ((Vh_tau_h-V)/k_tau2_h)))
  
  state n = 0.2932
  state h = 0.0912
  
  
  equations
    n' = (ninf - n)/tau_n
    h' = (hinf - h)/tau_h
    I = G_Kv4_s * n^4 * h * (E_K-V)
  end   
end
  /* ********************************************************************************************** */
