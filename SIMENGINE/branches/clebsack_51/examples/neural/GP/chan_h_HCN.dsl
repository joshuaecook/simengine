  /************************************************************************************************/
  // H current (heteromeric HCN1/2) Hyperpolarization activated Cation channels
model (I) = chan_h_HCN (V, G_H, E_H)
  
  //PARAMETER Vh_minf (-0.090 TO -0.060 BY 0.0005) = -0.0765 //V
  //PARAMETER k_minf (-0.010 TO -0.0005 BY 0.0001) = -0.0033 //V
  
  constant Vh_minf = -0.0764 //V
  constant k_minf = -0.0033 //V
  
  equation minf = 1/(1 + exp((Vh_minf - V)/k_minf))
  
  //PARAMETER Vh_tau_m (-0.085 TO -0.070 BY 0.001) = -0.076 //V
  //PARAMETER tau_m_min (0.001 TO 0.010 BY 0.001) = 0.001 //s // This is the simdefaults.g value divided by 4 (dq10_HCN1)
  //PARAMETER tau_m_max (3.5 TO 3.8 BY 0.01) = 3.63 //s // This is the simdefaults.g value divided by 4 (dq10_HCN1)
  //PARAMETER k_tau1_m (0.05 TO 0.08 BY 0.0005) = 0.0065 //V
  //PARAMETER k_tau2_m (-0.008 TO -0.007 BY 0.0005) = -0.0075 //V
  
  constant Vh_tau_m  = -0.0764 //V
  constant tau_m_min = 1e-5 //s // This is the simdefaults.g value divided by 4 (dq10_HCN1) // MUST NOT SET TO ZERO!!!
  constant tau_m_max = 3.625 //s // This is the simdefaults.g value divided by 4 (dq10_HCN1)
  constant k_tau1_m  = 0.00656 //V
  constant k_tau2_m  = -0.00748 //V
  
  state m = 0.0069

  equations
  //tau_m = (tau_m_max)/(exp((Vh_tau_m - V)/k_tau1_m) + exp((Vh_tau_m - V)/k_tau2_m))
    tau_m = tau_m_min + ((tau_m_max - tau_m_min)/(exp((Vh_tau_m - V)/k_tau1_m) + exp((Vh_tau_m - V)/k_tau2_m)))
  //tau_m = tau_m_max
  
  
    m' = (minf - m)/tau_m
  //g_bar = G_H
    I = G_H * m * (E_H - V)
  //I = g_bar * m * (E_H - V)
  end

end
  /************************************************************************************************/
  