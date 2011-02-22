/*  
 *   Leech heartbeat timing network model (timing network model)
 *   From Hill et al, 2001, J. Comp. Neuro
 *   Copyright 2007-2009 Simatra Modeling Technolgies
 *   Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)
 */

import "hn34.dsl"
import "synapse.dsl"

model (VmL3, VmR3) = hco(stimR4, Eleak, gleak)
	  input stimR4 with {default = 0}
      input Eleak with {default = -62.5}
      input gleak with {default = 11}

	  submodel hn34 HNL3 with {gleak = gleak, Eleak = Eleak}
	  submodel hn34 HNR3 with {gleak = gleak, Eleak = Eleak}

	  submodel synapse synapseR3L3 with {Vpre = HNR3.Vm, Vpost = HNL3.Vm, gSyn = 60}
	  submodel synapse synapseL3R3 with {Vpre = HNL3.Vm, Vpost = HNR3.Vm, gSyn = 60}

	  HNL3.ISyn = synapseR3L3.ISyn
	  HNR3.ISyn = synapseL3R3.ISyn

	  HNR3.IStim = stimR4

	  output VmL3 = HNL3.Vm
	  output VmR3 = HNR3.Vm
	 
	  solver = ode23
end
