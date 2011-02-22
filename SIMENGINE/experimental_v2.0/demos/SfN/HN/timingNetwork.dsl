import "hn34.dsl"
import "hn12.dsl"
import "synapse.dsl"

model (Vm) = timingNetwork(stimR4, gleak3, Eleak3, gh3, tauhCaS3)
	  input stimR4 with {default = 0}
      input gleak3 with {default = 8}
      input Eleak3 with {default = -62.5}
      input  gh3 with {default = 4}
	  input tauhCaS3 with {default = 5.25}

	  submodel hn34 HNL3 with {gleak = gleak3, Eleak = Eleak3, gh = gh3, tauhCaS = tauhCaS3}
	  submodel hn34 HNR3 with {gleak = gleak3, Eleak = Eleak3, gh = gh3, tauhCaS = tauhCaS3}
	  submodel hn34 HNL4 with {gleak = 11, Eleak = -62.45}
	  submodel hn34 HNR4 with {gleak = 11, Eleak = -62.45}

	  submodel hn12 HNRC
	  submodel hn12 HNLC

	  submodel synapse synapseR3L3 with {Vpre = HNR3.Vm, Vpost = HNL3.Vm, gSyn = 60}
	  submodel synapse synapseL3R3 with {Vpre = HNL3.Vm, Vpost = HNR3.Vm, gSyn = 60}
	  
	  submodel synapse synapseR4L4 with {Vpre = HNR4.Vm, Vpost = HNL4.Vm, gSyn = 60}
	  submodel synapse synapseL4R4 with {Vpre = HNL4.Vm, Vpost = HNR4.Vm, gSyn = 60}
	  
	  submodel synapse synapseR1R3 with {Vpre = HNRC.Vm, Vpost = HNR3.Vm, gSyn = 16}
	  submodel synapse synapseR1R4 with {Vpre = HNRC.Vm, Vpost = HNR4.Vm, gSyn = 16}
	  submodel synapse synapseR3R1 with {Vpre = HNR3.Vm, Vpost = HNRC.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}
	  submodel synapse synapseR4R1 with {Vpre = HNR4.Vm, Vpost = HNRC.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}

	  submodel synapse synapseL1L3 with {Vpre = HNLC.Vm, Vpost = HNL3.Vm, gSyn = 16}
	  submodel synapse synapseL1L4 with {Vpre = HNLC.Vm, Vpost = HNL4.Vm, gSyn = 16}
	  submodel synapse synapseL3L1 with {Vpre = HNL3.Vm, Vpost = HNLC.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}
	  submodel synapse synapseL4L1 with {Vpre = HNL4.Vm, Vpost = HNLC.Vm, gSyn = 6, tRise = 0.01, tFall = 0.055}

	  HNL3.ISyn = synapseR3L3.ISyn + synapseL1L3.ISyn
	  HNR3.ISyn = synapseL3R3.ISyn + synapseR1R3.ISyn

	  HNL4.ISyn = synapseR4L4.ISyn + synapseL1L4.ISyn
	  HNR4.ISyn = synapseL4R4.ISyn + synapseR1R4.ISyn

	  HNLC.ISyn = synapseL4L1.ISyn + synapseL3L1.ISyn
	  HNRC.ISyn = synapseR4R1.ISyn + synapseR3R1.ISyn

	  HNR4.IStim = stimR4

	  output Vm = (HNL3.Vm, HNR3.Vm, HNL4.Vm, HNR4.Vm, HNLC.Vm, HNRC.Vm)
	 
	  solver = forwardeuler
	  solver.dt = 1e-4
end
