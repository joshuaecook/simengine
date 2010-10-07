  
  /* ********************************************************************************************** */
  // Slow activating K Current Kv2 aka Slow delayed rectifier K channel
model (I) = chan_Kv2 (V, G_Kv2, E_K)
  
  //constant E_K =  //mV
  
  // fast activation gate "n"
  /*
  PARAMETER MIN_ninf (-0.5 TO 0.5 BY 0.01) = 0 // unitless
  PARAMETER Vh_ninf (-0.050 TO -0.020 BY 0.0001) = -0.0332 //V
  PARAMETER k_ninf (0.005 TO 0.010 BY 0.0001) = 0.0091 //V
  */
  
  constant MIN_ninf = 0 // unitless
  constant Vh_ninf = -0.0332 //V
  constant k_ninf = 0.0091 //V
  
  /*
  PARAMETER tau_n_min (0.0001 TO 0.001 BY 0.0001) = 0.0001 //s
  PARAMETER tau_n_max (0.020 TO 0.040 BY 0.005) = 0.030 //s
  PARAMETER Vh_tau_n (-0.050 TO -0.020 BY 0.0001) = -0.0332 //V
  PARAMETER k_tau1_n (0.015 TO 0.030 BY 0.0001) = 0.0217 //V
  PARAMETER k_tau2_n (-0.020 TO -0.010 BY 0.0001) = -0.0139 //V
  */
  
  constant tau_n_min = 0.0001 //s
  constant tau_n_max = 0.030 //s
  constant Vh_tau_n = -0.0332 //V
  constant k_tau1_n = 0.0217 //V
  constant k_tau2_n = -0.0139 //V
  
  equations
    ninf = MIN_ninf + ((1 - MIN_ninf)/(1 + exp ((Vh_ninf - V)/k_ninf)))
    tau_n = tau_n_min + ((tau_n_max - tau_n_min)/(exp ((Vh_tau_n - V)/k_tau1_n) + exp ((Vh_tau_n - V)/k_tau2_n)))
  end
  
  // fast inactivation gate "h"
  /*
  PARAMETER MIN_hinf (-0.5 TO 0.5 BY 0.01) = 0.2 //unitless
  PARAMETER Vh_hinf (-0.040 TO -0.010 BY 0.0005) = -0.020 //V
  PARAMETER k_hinf (-0.015 TO -0.005 BY 0.0005) = -0.010 //V
  PARAMETER tau_h_min (3 TO 4 BY 0.1) = 3.4 //s
  PARAMETER tau_h_max (3 TO 4 BY 0.1) = 3.4 //s
  */
  
  constant MIN_hinf = 0.2 //unitless
  constant Vh_hinf = -0.020 //V
  constant k_hinf = -0.010 //V
  
  constant tau_h_min = 3.4 //s
  
  // BECAUSE tau_h_min - tau_h_max -> tau_h = tau_h_min
  state n = 0.0497
  state h = 0.9856

  equations
    hinf = MIN_hinf + ((1-MIN_hinf)/(1 + exp ((Vh_hinf - V)/k_hinf)))
    tau_h = tau_h_min 
  
    n' = (ninf - n)/tau_n
    h' = (hinf - h)/tau_h
  
    I = G_Kv2 * n^4 * h * (E_K-V)
  end
  
end
  /* ********************************************************************************************** */
  
