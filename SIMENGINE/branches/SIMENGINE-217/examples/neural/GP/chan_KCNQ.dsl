  /* ********************************************************************************************** */
  // KCNQ current M-type K channel
model (I) = chan_KCNQ (V, G_KCNQ, E_K)
  
  //constant E_K = 71 //mV
  
  // fast activation gate "n"
  /*
  PARAMETER MIN_ninf (-0.5 TO 0.5 BY 0.01) = 0 // unitless
  PARAMETER Vh_ninf (-0.070 TO -0.050 BY 0.0001) = -0.061 //V
  PARAMETER k_ninf (0.010 TO 0.025 BY 0.0001) = 0.0195 //V
  */
  
  constant MIN_ninf = 0 // unitless
  constant Vh_ninf = -0.061 //V
  constant k_ninf = 0.0195 //V
  
  /*
  PARAMETER tau_n_min (0.001 TO 0.010 BY 0.0001) = 0.0067 //s
  PARAMETER tau_n_max (0.080 TO 0.120 BY 0.0005) = 0.100 //s
  PARAMETER Vh_tau_n (-0.080 TO -0.050 BY 0.0005) = -0.061 //V
  PARAMETER k_tau1_n (0.020 TO 0.040 BY 0.0001) = 0.035 //V
  PARAMETER k_tau2_n (-0.040 TO -0.020 BY 0.0001) = -0.025 //V
  */
  
  constant Vh_tau_n = -0.061 //V
  constant tau_n_min = 0.0067 //s
  constant tau_n_max = 0.100 //s
  constant k_tau1_n = 0.035 //V
  constant k_tau2_n = -0.025 //V
  
  state n = 0.5124

  equations
    ninf = MIN_ninf + ((1-MIN_ninf)/(1 + exp ((Vh_ninf - V)/k_ninf)))
    tau_n = tau_n_min + ((tau_n_max - tau_n_min)/(exp ((Vh_tau_n - V)/k_tau1_n) + exp ((Vh_tau_n-V)/k_tau2_n)))
  
    n' = (ninf - n)/tau_n
  
    I = G_KCNQ * n^4 * (E_K-V)
  end
  
end
  /* ********************************************************************************************** */
