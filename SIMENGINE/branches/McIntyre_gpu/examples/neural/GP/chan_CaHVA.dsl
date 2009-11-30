  /* ********************************************************************************************** */
  // High Voltage Activated Ca2+ current CaHVA
model (I) = chan_CaHVA (V, G_CaHVA, E_Ca)
  // in GENESIS this channel was created using the setuptau method. It solves the equation for both tau and minf:
  // y(x) = (A+B*x)/(C+exp((x+D)/F))
  
  /*
  PARAMETER Vh_minf (-0.050 TO -0.010 BY 0.0005) = -0.020 //V
  PARAMETER k_minf (-0.010 TO -0.001 BY 0.0005) = -0.007 //V
  PARAMETER tau_m (0.00005 TO 0.0005 BY 0.00001) = 0.0002 //s
  */
  
  constant Vh_minf = -0.020 //V
  constant k_minf  = -0.007 //V
  constant tau_m = 0.0002 //s

  state m = 0.0033
  
  equations
    tau = ((tau_m + tau_m * (1e-6) * V)/(exp(V/(1e6))))
    minf = (1/(1 + exp ((V + (-1 * Vh_minf))/k_minf)))
  
    m' = (minf - m)/tau
  
    I = G_CaHVA * m * (E_Ca-V)
  end

end
  /* ********************************************************************************************** */
