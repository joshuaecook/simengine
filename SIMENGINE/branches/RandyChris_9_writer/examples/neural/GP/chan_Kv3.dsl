  /* ********************************************************************************************** */
  // fast activating, incompletely inactivating K Current Kv3 aka fast delayed rectifer K channel
model (I) = chan_Kv3 (V, G_Kv3, E_K)
  
  //constant E_K =  //mV
  
  // fast activation gate "m"
  /*
  PARAMETER MIN_ninf (-0.5 TO 0.5 BY 0.01) = 0 // unitless
  PARAMETER Vh_ninf (-0.040 TO -0.020 BY 0.0005) = -0.026 //V
  PARAMETER k_ninf (0.002 TO 0.010 BY 0.0001) = 0.0078 //V
  */
  
  constant MIN_ninf = 0 // unitless
  constant Vh_ninf = -0.026 //V
  constant k_ninf = 0.0078 //V
  
  /*
  PARAMETER tau_n_min (0.0001 TO 0.001 BY 0.0001) = 0.0001 //s
  PARAMETER tau_n_max (0.010 TO 0.025 BY 0.0005) = 0.014 //s
  PARAMETER Vh_tau_n (-0.040 TO -0.020 BY 0.0005) = -0.026 //V
  PARAMETER k_tau1_n (-0.020 TO -0.010 BY 0.001) = -0.012 //V
  PARAMETER k_tau2_n (0.010 TO 0.020 BY 0.001) = 0.013 //V  //BE CAREFUL, J. EDGERTON FLIPPED SOME SIGNS
  */
  
  constant tau_n_min = 0.0001 //s
  constant tau_n_max = 0.014 //s
  constant Vh_tau_n = -0.026 //V
  constant k_tau1_n = -0.012 //V
  constant k_tau2_n = 0.013 //V  //BE CAREFUL, J. EDGERTON FLIPPED SOME SIGNS

  equations  
    ninf = MIN_ninf + ((1-MIN_ninf)/(1 + exp ((Vh_ninf - V)/k_ninf)))
    tau_n = tau_n_min + ((tau_n_max - tau_n_min)/(exp ((Vh_tau_n - V)/k_tau1_n) + exp ((Vh_tau_n - V)/k_tau2_n)))
  end
  
  // fast inactivation gate "h"
  /*
  PARAMETER MIN_hinf (0.01 TO 0.7 BY 0.01) = 0.6 // unitless
  PARAMETER Vh_hinf (-0.040 TO -0.010 BY 0.0005) = -0.020 //V
  PARAMETER k_hinf (-0.015 TO -0.005 BY 0.0005) = -0.010 //V
  */
  
  constant MIN_hinf = 0.6 // unitless
  constant Vh_hinf = -0.020 //V
  constant k_hinf = -0.010 //V
  
  /*
  PARAMETER tau_h_min (0.001 TO 0.010 BY 0.001) = 0.007 //s
  PARAMETER tau_h_max (0.030 TO 0.040 BY 0.001) = 0.033 //s
  PARAMETER Vh_tau_h (-0.020 TO 0.020 BY 0.001) = 0 //V
  PARAMETER k_tau1_h (0.001 TO 0.020 BY 0.0005) = 0.010 //V
  PARAMETER k_tau2_h (-0.020 TO -0.001 BY 0.0005) = -0.010 //V
  */
  
  constant tau_h_min = 0.007 //s
  constant tau_h_max = 0.033 //s
  constant Vh_tau_h = 0 //V
  constant k_tau1_h = 0.010 //V
  constant k_tau2_h = -0.010 //V
  
  state n = 0.0126
  state h = 0.9928

  equations
    hinf = MIN_hinf + ((1-MIN_hinf)/(1 + exp ((Vh_hinf - V)/k_hinf)))
    tau_h = tau_h_min + ((tau_h_max - tau_h_min)/(exp ((Vh_tau_h - V)/k_tau1_h) + exp ((Vh_tau_h-V)/k_tau2_h)))
    
    n' = (ninf - n)/tau_n
    h' = (hinf - h)/tau_h
  
    I = G_Kv3 * n^4 * h * (E_K-V)
  end
  
end
  /* ********************************************************************************************** */
  