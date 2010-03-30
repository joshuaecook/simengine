/*
 * RLC - demo model showing a simple RLC electrical circuit
 * 
 * Copyright 2010 Simatra Modeling Technologies
 */

import "circuit_elements.dsl"

model (V, I) = rlc(R, C, L)

  // Define component values with default values
  input R with {default=1e2}     // Ohms
  input C with {default=1e-6}    // Farads
  input L with {default=10}      // Henry

  // Create an input voltage waveform which will be driven by a voltage source
  equation Vinp = {0 when t < 0.1,
		   10 when t < 1,
		   -10 when t < 1.9,
		   0 otherwise}

  // Instantiate submodels
  submodel Resistor R1 with {R=R}
  submodel Capacitor C1 with {C=C}
  submodel Inductor L1 with {L=L}
  submodel VoltageSource Vs with {Vs=Vinp}      
  submodel Ground Gnd

  // Define a state for each potential in the system
  state Va = 0
  state Vb = 0
  state Vc = 0
  state Vgnd = 0

  // Provide some capacitance to store charge at a junction
  constant Cnode = 1e-16 

  // Define each of the node equations as storing and releasing charge on a junction 
  // capacitor
  equations
    Va' = 1/Cnode * (Vs.I - R1.I)
    Vb' = 1/Cnode * (R1.I - C1.I)
    Vc' = 1/Cnode * (C1.I - L1.I)
    Vgnd' = 1/Cnode * (L1.I - Gnd.I - Vs.I)
  end

  // Connect up the circuit
  Vs.V1 = Vgnd
  Vs.V2 = Va
  R1.V1 = Va
  R1.V2 = Vb
  C1.V1 = Vb
  C1.V2 = Vc
  L1.V1 = Vc
  L1.V2 = Vgnd
  Gnd.V1 = Vgnd
  
  // Define both a voltage and a current output as a grouping of individual values
  output V = (Va, Vb, Vc)
  output I = (Vs.I, R1.I, L1.I, C1.I)

  // Use a backward-Euler implicit scheme to simulate this circuit.
  solver=linearbackwardeuler{dt=0.0001}

end
