/*  
 *   Leech heartbeat timing network model (timing network model)
 *   From Hill et al, 2001, J. Comp. Neuro
 *   Copyright 2007-2009 Simatra Modeling Technolgies
 *   Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)
 */

model (ISyn) = synapse(Vpre, Vpost, gSyn, tRise, tFall)
    constant Vthresh = -20   //mV
    input gSyn with {default = 60}   //nS   
    input Vpre with {default = 0}
    input Vpost with {default = 0}
    
    constant ESyn = -62.5
    input tRise with {default = 0.002}
    input tFall with {default =  0.011}
    
    state fSyn = 0
    state MSyn = 0.1

    equations
	// Specify a function for tau
	taux(a, b, c, e, V) => c + e / (1 + exp(a * (V + b)))

	fSyn' = {(0.999-fSyn)/tRise    when Vpre > Vthresh, 
		 -fSyn/tFall           otherwise}
	MSyn' = (taux(-0.99, 40, 0.1, 0.9, Vpre) - MSyn)/0.2
	
	// Computing the output synaptic currect
	ISyn = gSyn * fSyn * MSyn * (Vpost - ESyn)
    end

    solver = forwardeuler
    solver.dt = 1e-4
end
