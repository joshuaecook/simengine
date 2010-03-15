/*
 * RLC - demo model showing a simple RLC electrical circuit
 * 
 * Copyright 2010 Simatra Modeling Technologies
 */

settings.compiler.verbose.setValue(true)
settings.ir.aggregate.setValue(true)
constant DT = 0.0001
import "circuit_elements.dsl"

model (V, I) = rlc(R1, R2, C, L)

      input R1 with {default=1e3}   // Ohms
      input R2 with {default=1e6}   // Ohms
      input C with {default=1e-6}   // Farads
      input L with {default=1}      // Henry

      // Create an input
      equation Vinp = {0 when t < 0.1,
		       10 when t < 1,
		       -10 when t < 1.9,
		       0 otherwise}

      // Instantiate submodels
      submodel Resistor Res1 with {R=R1}
      submodel Resistor Res2 with {R=R2}
      submodel Capacitor Cap1 with {C=C}
      submodel Inductor Ind1 with {L=L}
      submodel VoltageSource Vs with {Vs=Vinp}
      submodel Ground Gnd

      constant Vgnd = 0

      // Define a state for each potential in the system
      state Va = 0
      state Vb = 0
      state Vc = 0
      state Vgnd = 0

      constant Cnode = 1e-16
      equations
	Va' = 1/Cnode * (Vs.I - Res1.I)
	Vb' = 1/Cnode * (Res1.I - Cap1.I - Res2.I)
	Vc' = 1/Cnode * (Cap1.I + Res2.I - Ind1.I)
	Vgnd' = 1/Cnode (Ind1.I - Gnd.I - Vs.I)
      end


      //createNode(Va, [Vs.B, R1.A, R3.A])
      //createNode(Vb, [R1.B, R2.A])

      // Connect up the circuit
      Vs.V1 = Vgnd
      Vs.V2 = Va
      Res1.V1 = Va
      Res1.V2 = Vb
      Res2.V1 = Vb
      Res2.V2 = Vc
      Cap1.V1 = Vb
      Cap1.V2 = Vc
      Ind1.V1 = Vc
      Ind1.V2 = Vgnd
      Vgnd.V1 = Vgnd
      

      output V = (Va, Vb, Vc)
//      output I = (Vs.I, R1.I, L1.I, C1.I)
	output I = (Vs.I)

      solver=linearbackwardeuler{dt=DT}

end
