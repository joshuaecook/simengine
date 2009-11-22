/*  
 *   Leech heartbeat timing network model (timing network model)
 *   From Hill et al, 2001, J. Comp. Neuro
 *   Copyright 2007-2009 Simatra Modeling Technolgies
 *   Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)
 */

import "hn34.dsl"
import "hn12.dsl"
import "synapse.dsl"

model (VmL3, VmR3, VmL4, VmR4, VmL1, VmR1) = timingNetwork(stimR4)
	  input stimR4 with {default = 0}

	  submodel hn34 HNL3 with {gleak = 11, Eleak = -62.5}
	  submodel hn34 HNR3 with {gleak = 11, Eleak = -62.4}
	  submodel hn34 HNL4 with {gleak = 11, Eleak = -62.5}
	  submodel hn34 HNR4 with {gleak = 11, Eleak = -62.4}

	  submodel hn12 HNR1
	  submodel hn12 HNL1

	  submodel synapse synapseR3L3 with {Vpre = HNR3.Vm, Vpost = HNL3.Vm, gSyn = 60}
	  submodel synapse synapseL3R3 with {Vpre = HNL3.Vm, Vpost = HNR3.Vm, gSyn = 60}
	  
	  submodel synapse synapseR4L4 with {Vpre = HNR4.Vm, Vpost = HNL4.Vm, gSyn = 60}
	  submodel synapse synapseL4R4 with {Vpre = HNL4.Vm, Vpost = HNR4.Vm, gSyn = 60}
	  
	  submodel synapse synapseR1R3 with {Vpre = HNR1.Vm, Vpost = HNR3.Vm, gSyn = 16}
	  submodel synapse synapseR1R4 with {Vpre = HNR1.Vm, Vpost = HNR4.Vm, gSyn = 16}
	  submodel synapse synapseR3R1 with {Vpre = HNR3.Vm, Vpost = HNR1.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}
	  submodel synapse synapseR4R1 with {Vpre = HNR4.Vm, Vpost = HNR1.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}

	  submodel synapse synapseL1L3 with {Vpre = HNL1.Vm, Vpost = HNL3.Vm, gSyn = 16}
	  submodel synapse synapseL1L4 with {Vpre = HNL1.Vm, Vpost = HNL4.Vm, gSyn = 16}
	  submodel synapse synapseL3L1 with {Vpre = HNL3.Vm, Vpost = HNL1.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}
	  submodel synapse synapseL4L1 with {Vpre = HNL4.Vm, Vpost = HNL1.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}

	  HNL3.ISyn = synapseR3L3.ISyn + synapseL1L3.ISyn
	  HNR3.ISyn = synapseL3R3.ISyn + synapseR1R3.ISyn

	  HNL4.ISyn = synapseR4L4.ISyn + synapseL1L4.ISyn
	  HNR4.ISyn = synapseL4R4.ISyn + synapseR1R4.ISyn

	  HNL1.ISyn = synapseL4L1.ISyn + synapseL3L1.ISyn
	  HNR1.ISyn = synapseR4R1.ISyn + synapseR3R1.ISyn

	  HNR4.IStim = stimR4

	  output VmL3 = HNL3.Vm
	  output VmR3 = HNR3.Vm
	  output VmL4 = HNL4.Vm
	  output VmR4 = HNR4.Vm
	  output VmL1 = HNL1.Vm
	  output VmR1 = HNR1.Vm
	 
	  solver = cvode//forwardeuler//ode23//forwardeuler
	  solver.dt = 1e-4
end
