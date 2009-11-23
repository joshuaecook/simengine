// Copyright 2009 Simatra Modeling Technologies

import "neuron.dsl"

model (Vm1, Vm2) = twoCellNetwork(stimulusCurrent)
	submodel neuron neuron1 with {IStim = stimulusCurrent}
	submodel neuron neuron2

	submodel synapse mySynapse with {Vpre = neuron1.Vm, Vpost = neuron2.Vm,
			 		 Vthresh = -20, ESyn = -60, gSyn = 10}

	neuron2.IStim = mySynapse.ISyn

	output Vm1 = neuron1.Vm
	output Vm2 = neuron2.Vm

    solver = forwardeuler
	solver.dt = .001
end



