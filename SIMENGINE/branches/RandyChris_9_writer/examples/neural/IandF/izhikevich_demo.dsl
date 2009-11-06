import "izhikevich.dsl"

model (v_RS, v_IB, v_CH, v_FS, v_LTS, v_RE) = izhikevich_demo(I)

    submodel izhikevich regular_spiking with {I=I, d=8}
    submodel izhikevich intrinsically_bursting with {I=I, c=-55, d=4}
    submodel izhikevich chattering with {I=I, c=-50, d=2}
    submodel izhikevich fast_spiking with {I=I, a=0.1}
    submodel izhikevich low_threshold_spiking with {I=I, b=0.25}
//    submodel izhikevich thalamo_cortical with {}
    submodel izhikevich resonator with {I=I, a=0.1, b=0.26}

    equations
	v_RS = regular_spiking.v
	v_IB = intrinsically_bursting.v
	v_CH = chattering.v
	v_FS = fast_spiking.v
	v_LTS = low_threshold_spiking.v
	v_RE = resonator.v
    end

end
