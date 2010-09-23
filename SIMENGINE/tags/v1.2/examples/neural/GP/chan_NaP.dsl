  /************************************************************************************************/
  // Persistent Sodium Current NaP
model (I) = chan_NaP (V, G_NaP, E_Na)
  
  // fast activation gate "m"
  /*
  PARAMETER MIN_minf (-0.5 TO 0.5 BY 0.01) = 0 //unitless
  PARAMETER Vh_minf (-0.070 TO -0.040 BY 0.0001) = -0.0577 //V
  PARAMETER k_minf (0.002 TO 0.010 BY 0.0001) = 0.0057 //V
  */
  
  state m = 0.3990
  state h = 0.7280

  constant MIN_minf = 0 //unitless
  constant Vh_minf = -0.0577 //V
  constant k_minf = 0.0057 //V
  
  /*
  PARAMETER Vh_tau_m (-0.060 TO -0.030 BY 0.0001) = -0.0426 //V
  PARAMETER tau_m_min (0.00001 TO 0.001 BY 0.00001) = 0.00001 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  PARAMETER tau_m_max (0.00001 TO 0.001 BY 0.00001) = 0.00015 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  PARAMETER k_tau1_m (0.010 TO 0.020 BY 0.0001) = 0.0144 //V
  PARAMETER k_tau2_m (-0.020 TO -0.010 BY 0.0001) = -0.0144 //V
  */
  
  constant Vh_tau_m = -0.0416 //V
  constant tau_m_min = 0.00001 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  constant tau_m_max = 0.0001456 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  constant k_tau1_m = 0.0144 //V
  constant k_tau2_m = -0.0144 //V
  
  equation minf = MIN_minf + ((1-MIN_minf)/(1 + exp((Vh_minf - V)/k_minf)))
  equation tau_m = tau_m_min + ((tau_m_max - tau_m_min)/(exp ((Vh_tau_m - V)/k_tau1_m) + exp((Vh_tau_m - V)/k_tau2_m)))
  
  // fast inactivation gate "h"
  /*
  PARAMETER MIN_hinf (0.001 TO 0.3 BY 0.001) = 0.154 //unitless
  PARAMETER Vh_hinf (-0.070 TO -0.030 BY 0.0005) = -0.057 //V
  PARAMETER k_hinf (-0.007 TO -0.002 BY 0.0001) = -0.004 //V
  */
  
  constant MIN_hinf = 0.154 //unitless
  constant Vh_hinf = -0.057 //V
  constant k_hinf = -0.004 //V
  
  /*
  PARAMETER tau_h_min (0.01 TO 0.05 BY 0.001) = 0.01 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  PARAMETER tau_h_max (0.01 TO 0.05 BY 0.001) = 0.017 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  PARAMETER Vh_tau_h (-0.060 TO -0.020 BY 0.0005) = -0.034 //V
  PARAMETER k_tau1_h (0.015 TO 0.030 BY 0.0005) = 0.026 //V
  PARAMETER k_tau2_h (-0.040 TO -0.025 BY 0.0001) = -0.0319 //V
  */
  
  constant tau_h_min = 0.01 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  constant tau_h_max = 0.017 //s // This is the simdefaults.g value divided by 3 (dq10_NaP)
  constant Vh_tau_h = -0.034 //V
  constant k_tau1_h = 0.026 //V
  constant k_tau2_h = -0.0319 //V
  
  equation hinf = MIN_hinf + ((1-MIN_hinf)/(1 + exp ((Vh_hinf - V)/k_hinf)))
  equation tau_h = tau_h_min + ((tau_h_max - tau_h_min)/(exp ((Vh_tau_h - V)/k_tau1_h) + exp ((Vh_tau_h - V)/k_tau2_h)))
  
  equations
    m' = (minf - m)/tau_m
    h' = (hinf - h)/tau_h
  //d(s) = (sinf - s)/tau_s
  
  //I = G_NaP * cube(m) * h * s * (E_Na - V)
    I = G_NaP * m^3 * h * (E_Na - V)
  end

  // slow inactivation gate "s" 
  // NOTE: J. EDGERTON SAID TAKING THIS OUT HAS LITTLE EFFECT 
  // LIAR LIAR LIAR LIAR LIAR
  /*
  PARAMETER MIN_sinf (-0.5 TO 0.5 BY 0.001) = 0 //unitless
  PARAMETER Vh_sinf (-20 TO 20 BY 0.5) = -10 //mV
  PARAMETER k_sinf (-6 TO -3 BY 0.1) = -4.9 //mV
  */
  
  /*
  constant MIN_sinf = 0 //unitless
  constant Vh_sinf = -0.010 //V
  constant k_sinf = -0.0049 //V
  
  constant A_alpha = -2.88 // V^-1 s^-1
  constant B_alpha = -0.049 // s^-1
  constant K_alpha = 0.00463 //V
  
  constant A_beta = 6.94 //V^-1 s^-1
  constant B_beta = 0.447 //s^-1
  constant K_beta = -0.00263 //V
  
  sinf = MIN_sinf + ((1-MIN_sinf)/(1 + exp ((Vh_sinf - V)/k_sinf)))
  tau_s_alpha = (A_alpha * V + B_alpha)/(1-exp((V + B_alpha/A_alpha)/K_alpha))
  tau_s_beta = (A_beta * V + B_beta)/(1-exp((V + B_beta/A_beta)/K_beta))
  tau_s = 1/(3*(tau_s_alpha + tau_s_beta))
  */
  
  //STATE s (0 TO 1 BY 0.0001) = 1
  
  
end
  /************************************************************************************************/
