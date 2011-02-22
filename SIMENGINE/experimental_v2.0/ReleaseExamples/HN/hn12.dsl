/*  
 *   Leech heartbeat timing network model (timing network model)
 *   From Hill et al, 2001, J. Comp. Neuro
 *   Copyright 2007-2009 Simatra Modeling Technolgies
 *   Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)
 */


//***************************************** HN 12 ***********************************************//
model (Vm) = hn12(gleak, Eleak, ISyn, IStim)
	//Membrane Capacitance
	constant Cmem = 0.5 //pF

	//Maximal conductances in nS
	constant gNa = 200
	constant gK1 = 150
	constant gK2 = 75
	input gleak with {default = 10}

	constant ENa = 45 //mV
	constant EK = -70
	input Eleak with {default = -40}

	input ISyn with {default = 0}
	input IStim with {default = 0}

	//State Variable Declaration
	state Vm = -45
	state hNa = 0.99 
	state mK1 = 0.03
	state hK1 = 0.81
	state mK2 = 0.16

	// Helper functions
	equations
	    xinf(a, b, V) => 1/(1 + exp(a * (V + b)))
	    taux(a, b, c, e, V) => c + e / (1 + exp(a * (V + b)))
	end

	//Ionic Currents
	equations
		mNa = xinf(-0.150, 29, Vm)
		INa = gNa*mNa^3*hNa*(Vm - ENa)
		IK1 = gK1*mK1^2*hK1*(Vm - EK)
		IK2 = gK2*mK2^2*(Vm - EK)
		Ileak = gleak*(Vm-Eleak)
	end

// Differential Equations
	equations
	  	hNa' = (xinf(0.500, 30, Vm) - hNa) / (0.004 + 0.006/(1 + exp(0.5*(Vm + 28))) + 0.01/(cosh(0.3*(Vm + 27))))
		mK1' = (xinf(-0.143, 21, Vm) - mK1) / taux(.150, 16, .001,.011, Vm)
  		hK1' = (xinf(.111, 28, Vm) - hK1) / taux(-.143, 13, 0.5, 0.2, Vm)
  		mK2' =(xinf(-0.083, 20, Vm) - mK2) / taux(0.2, 35, 0.057, 0.043, Vm)
  		Vm' = 1/Cmem*(-INa-IK1-IK2-Ileak+IStim-ISyn)
	end

solver = forwardeuler
solver.dt = 1e-4
end
