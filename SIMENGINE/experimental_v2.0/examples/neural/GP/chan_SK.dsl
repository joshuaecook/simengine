  /************************************************************************************************/
  // small conductance calcium dependent potassium current SK
model (I) = chan_SK (V, G_SK, E_K, Ca_in)
  
  //PARAMETER EC50 (0.00010 TO 0.00060 BY 0.00001) = 0.00035 //mM default = 350 nM
  //PARAMETER hill_slope (3 TO 6 BY 0.1) = 4.6
  
  constant EC50 = 0.00035
  constant hill_slope = 4.6
  
  /*
  PARAMETER tau_max (0.070 TO 0.080 BY 0.001) = 0.076 //s
  PARAMETER tau_min (0.001 TO 0.050 BY 0.001) = 0.004 //s
  */
  
  //PARAMETER Ca_sat (0.003 TO 0.007 BY 0.0001) = 0.005 //mM
  
  // THIS CAUSED ERROR, FIND OUT WHY
  constant tau_max = 0.076 //s
  constant tau_min = 0.004 //s
  constant Ca_sat  = 0.005 //mM

  state z = 0.0001
  equations
    tau_K = (tau_max - tau_min)/Ca_sat 
  
    z_inf =  (Ca_in^hill_slope)/(Ca_in^hill_slope) + EC50^hill_slope
  
    tau_z = {(tau_max - Ca_in * tau_K) when Ca_in < Ca_sat,
     	     tau_min otherwise}
/*
 (Ca_in < Ca_sat) THEN
  		//tau_max
  		(tau_max - Ca_in * tau_K) // this causes  a warning of a divide over zero, but it doesn't since it only occurs when Ca_in < Ca_sat
  	ELSE
  		tau_min
  */
  
    z' = 2*(z_inf - z)/tau_z
  
  //d(z) = z_inf
  //d(z) = tau_z
  
    I = G_SK * z * (E_K-V)
  end
  
end
  /************************************************************************************************/
