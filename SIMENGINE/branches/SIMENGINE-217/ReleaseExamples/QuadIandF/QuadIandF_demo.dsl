//
// Izhikevich Quadratic Integrate and Fire
// Izhikevich, E.M., Simple Model of Spiking Neurons, IEEE Trans on Neural Networks, Nov 2003
// Adapted for use with simEngine
// 
// Copyright 2009 Simatra Modeling Technologies
//

import "QuadIandF.dsl"

model (v_RS, v_IB, v_CH, v_FS, v_LTS, v_RN) = QuadIandF_demo(I)

    submodel QuadIandF regular_spiking with {I=I, d=8}
    submodel QuadIandF intrinsically_bursting with {I=I, c=-55, d=4}
    submodel QuadIandF chattering with {I=I, c=-50, d=2}
    submodel QuadIandF fast_spiking with {I=I, a=0.1}
    submodel QuadIandF low_threshold_spiking with {I=I, b=0.25}
    submodel QuadIandF resonator with {I=I, a=0.1, b=0.26}

    equations
	v_RS = regular_spiking.v
	v_IB = intrinsically_bursting.v
	v_CH = chattering.v
	v_FS = fast_spiking.v
	v_LTS = low_threshold_spiking.v
	v_RN = resonator.v
    end

end
