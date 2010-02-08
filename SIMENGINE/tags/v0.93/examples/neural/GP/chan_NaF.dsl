  /* ********************************************************************************************** */
  // Fast Sodium Current NaF
model (I) = chan_NaF (V, G_NaF, E_Na)
  
  // fast activation gate "m"
  /*
  PARAMETER MIN_minf (-0.5 TO 0.5 BY 0.001) = 0
  PARAMETER Vh_minf (-0.060 TO -0.020 BY 0.0005) = -0.039 // V //NOTE THIS MATCHES V0m in GENESIS GPchans.g
  PARAMETER k_minf (0.002 TO 0.010 BY 0.0005) = 0.005 // V
  */
  
  constant MIN_minf = 0
  constant Vh_minf = -0.039 // V //NOTE THIS MATCHES V0m in GENESIS GPchans.g
  constant k_minf = 0.005 // V
  
  //PARAMETER tau_m_min (0.010E-3 TO 0.040E-3 BY 0.001E-3) = 0.028E-3 // s
  //PARAMETER tau_m_max (0.010E-3 TO 0.040E-3 BY 0.001E-3) = 0.028E-3 // s
  
  constant tau_m_min = 0.028E-3 // s
  
  // BECAUSE tau_m_min = tau_m_max -> tau_m = tau_m_min
  equation minf = MIN_minf + ((1-MIN_minf)/(1 + exp ((Vh_minf - V)/k_minf)))
  equation tau_m = tau_m_min
  
  // fast inactivation gate "h"
  /*
  PARAMETER MIN_hinf (-0.5 TO 0.5 BY 0.001) = 0
  PARAMETER Vh_hinf (-0.060 TO -0.020 BY 0.0005) = -0.048 //V
  PARAMETER k_hinf (-0.005 TO -0.001 BY 0.0001) = -0.0028 //V
  */
  
  constant MIN_hinf = 0
  constant Vh_hinf = -0.048 //V
  constant k_hinf = -0.0028 //V
  
  /*
  PARAMETER tau_h_min (0.1E-3 TO 0.4E-3 BY 0.01E-3) = 0.25E-3 // s
  PARAMETER tau_h_max (2E-3 TO 6E-3 BY 0.1E-3) = 4E-3 // s
  PARAMETER Vh_tau_h (-0.060 TO -0.020 BY 0.001) = -0.043 //V
  PARAMETER k_tau1_h (0.002 TO 0.020 BY 0.0005) = 0.01 //V
  PARAMETER k_tau2_h (-0.010 TO -0.002 BY 0.0005) = -0.005 //V
  */
  
  constant tau_h_min = 0.25E-3 // s
  constant tau_h_max = 4E-3 // s
  constant Vh_tau_h = -0.043 //V
  constant k_tau1_h = 0.01 //V
  constant k_tau2_h = -0.005 //V
  
  equation hinf = MIN_hinf + ((1-MIN_hinf)/(1 + exp ((Vh_hinf - V)/k_hinf)))
  equation tau_h = tau_h_min + ((tau_h_max - tau_h_min)/(exp ((Vh_tau_h - V)/k_tau1_h) + exp ((Vh_tau_h - V)/k_tau2_h)))
  //tau_h = (1/(exp ((Vh_tau_h - V)) + exp ((Vh_tau_h-V))))
  
  // slow inactivation gate "s"
  /*
  PARAMETER MIN_sinf (0.01 TO 1 BY 0.01) = 0.15 // unitless
  PARAMETER Vh_sinf (-0.060 TO -0.020 BY 0.001) = -0.04 //V
  PARAMETER k_sinf (-0.007 TO -0.002 BY 0.0001) = -0.0054 //V
  */
  
  constant MIN_sinf = 0.15 // unitless
  constant Vh_sinf = -0.04 //V
  constant k_sinf = -0.0054 //V
  
  /*
  PARAMETER tau_s_min (5E-3 TO 15E-3 BY 0.5E-3) = 10E-3 // s
  PARAMETER tau_s_max (0.5 TO 1.5 BY 0.1) = 1 //s
  PARAMETER Vh_tau_s (-0.060 TO -0.020 BY 0.001) = -0.04 //V
  PARAMETER k_tau1_s (0.010 TO 0.020 BY 0.0001) = 0.0183 //V
  PARAMETER k_tau2_s (-0.015 TO -0.001 BY 0.0005) = -0.01 //V
  */
  
  constant tau_s_min = 10E-3 // s
  constant tau_s_max = 1 //s
  constant Vh_tau_s = -0.04 //V
  constant k_tau1_s = 0.0183 //V
  constant k_tau2_s = -0.01 //V
  
  equation sinf = MIN_sinf + ((1-MIN_sinf)/(1 + exp ((Vh_sinf - V)/k_sinf)))
  equation tau_s = tau_s_min + ((tau_s_max - tau_s_min)/(exp ((Vh_tau_s - V)/k_tau1_s) + exp ((Vh_tau_s - V)/k_tau2_s)))
  
  state m = 0.0153
  state h = 0.9863
  state s = 0.9795
  
  equations 
    m' = (minf - m)/tau_m
    h' = (hinf - h)/tau_h
    s' = (sinf - s)/tau_s
  
    I = G_NaF * m^3 * h * s * (E_Na-V)
  end
  
end
  /* ********************************************************************************************** */
