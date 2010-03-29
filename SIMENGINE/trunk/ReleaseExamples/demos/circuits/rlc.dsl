/*
 * RLC - demo model showing a simple RLC electrical circuit
 * 
 * Copyright 2010 Simatra Modeling Technologies
 */

import "circuit_elements.dsl"

model (V, I) = rlc(R, C, L)

  input R with {default=1e2}   // Ohms
  input C with {default=1e-6}   // Farads
  input L with {default=10}      // Henry

  // Create an input
  equation Vinp = {0 when t < 0.1,
		   10 when t < 1,
		   -10 when t < 1.9,
		   0 otherwise}

  // Instantiate submodels
  submodel Resistor Res1 with {R=R}
  submodel Capacitor Cap1 with {C=C}
//  submodel Resistor Cap1 with {R=R}
  submodel Inductor Ind1 with {L=L}
//  submodel Resistor Ind1 with {R=0.1}
  submodel VoltageSource Vs with {Vs=Vinp}      
  submodel Ground Gnd

  // Define a state for each potential in the system
  state Va = 0
  state Vb = 0
  state Vc = 0
  state Vgnd = 0

  constant Cnode = 1e-16 // provide some capacitance to store charge at a junction
  equations
    Va' = 1/Cnode * (Vs.I - Res1.I)
    Vb' = 1/Cnode * (Res1.I - Cap1.I)
    Vc' = 1/Cnode * (Cap1.I - Ind1.I)
    Vgnd' = 1/Cnode * (Ind1.I - Gnd.I - Vs.I)
  end


  // Connect up the circuit
  Vs.V1 = Vgnd
  Vs.V2 = Va
  Res1.V1 = Va
  Res1.V2 = Vb
  Cap1.V1 = Vb
  Cap1.V2 = Vc
  Ind1.V1 = Vc
  Ind1.V2 = Vgnd
  Gnd.V1 = Vgnd
  

  output V = (Va, Vb, Vc)
  output I = (Vs.I, Res1.I, Ind1.I, Cap1.I)

  solver=linearbackwardeuler{dt=0.0001}

end
