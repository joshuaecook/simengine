import "neuronWithSynapse.dsl"

model (Vm1, Vm2) = twoCellNetwork(IStim)
	input IStim with {default = 0}
	submodel neuron neuron1 with {Iext = IStim}
	submodel neuron neuron2

	submodel synapse synapse1 with {Vpre = neuron1.Vm, Vpost = neuron2.Vm, 
				gSyn = 1, ESyn = -60, Vthresh = -20}

	neuron2.Iadd = synapse1.ISyn

	output Vm1 = neuron1.Vm
	output Vm2 = neuron2.Vm

   solver = forwardeuler
	solver.dt = .001
end



