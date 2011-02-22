  /************************************************************************************************/
  // H current (homomeric HCN2) Hyperpolarization activated Cation channels
model (I) = chan_h_HCN2 (V, G_H, E_H)
  
  /*
  PARAMETER Vh_minf (-0.090 TO -0.060 BY 0.0005) = -0.0875 //V
  PARAMETER k_minf (-0.010 TO -0.0005 BY 0.0001) = -0.004 //V
  */
  
  constant Vh_minf = -0.0875 //V
  constant k_minf = -0.004 //V
  
  equation minf = 1/(1 + exp((Vh_minf - V)/k_minf))
  
  //PARAMETER Vh_tau_m (-0.090 TO -0.060 BY 0.0001) = -0.0875 //V
  //PARAMETER tau_m_min (0.001 TO 0.010 BY 0.0005) = 0.001 //s // This is the simdefaults.g value divided by 4 (dq10_HCN2)
  //PARAMETER tau_m_max (5 TO 10 BY 0.1) = 6.3 //s // This is the simdefaults.g value divided by 4 (dq10_HCN2)
  //PARAMETER k_tau1_m (0.001 TO 0.010 BY 0.0001) = 0.0089 //V
  //PARAMETER k_tau2_m (-0.010 TO -0.001 BY 0.0001) = -0.0082 //V
  
  constant Vh_tau_m = -0.0875 //V
  constant tau_m_min = 1e-5 //s // This is the simdefaults.g value divided by 4 (dq10_HCN2)
  constant tau_m_max = 6.3 //s // This is the simdefaults.g value divided by 4 (dq10_HCN2)
  constant k_tau1_m = 0.0089 //V
  constant k_tau2_m = -0.0082 //V
  
  state m = 0.0010

  equations
    tau_m = tau_m_min + ((tau_m_max - tau_m_min)/(exp((Vh_tau_m - V)/k_tau1_m) + exp((Vh_tau_m - V)/k_tau2_m)))
  
  
    m' = (minf - m)/tau_m
  //g_bar = G_H
    I = G_H * m * (E_H - V)
  //I = g_bar * m * (E_H - V)
  end

end
  /************************************************************************************************/
