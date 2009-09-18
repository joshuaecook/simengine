 function xinf(a, b) = 1/(1+exp((Vm + a)/b))
 function tau(a, b, c, d) = a/(exp((Vm+b)/c) + exp(-(Vm+b)/d))

model (Vm) = neuron(Iadd, Iext)
      input Iext with {default = 0}
      input Iadd with {default = 0}

      constant Cm = 1

      constant gNa = 120
      constant gK = 100
      constant gleak = .51

      constant ENa = 55
      constant EK = -80
      constant Eleak = -55

      state Vm = -45
      state hNa = 0.9
      state mK = 0.1

      equations     
 	    INa = gNa*xinf(35, -7.8)^3*hNa*(Vm - ENa)
     	IK = gK*mK^4*(Vm - EK)
     	Ileak = gleak*(Vm - Eleak)

     	hNa' = (xinf(55, 7) - hNa)/tau(30, 50, 15, 16)
     	mK' = (xinf(28, -15) - mK)/tau(7, 40, 40, 50)
     	Vm' = -(1/Cm)*(INa + IK + Ileak+ Iadd - Iext)
      end

      solver = forwardeuler
      solver.dt = .001
end

model (ISyn) = synapse(Vpre, Vpost, gSyn, ESyn, Vthresh)
	state m = 0.1
	constant tau = 0.5
    equation minf = 1/(1+exp((Vpre+28)/-15))
	equation m' = (minf - m)/tau

	output ISyn = {(gSyn*m*(Vpost-ESyn)) when Vpre > Vthresh,
						0 otherwise}
end

/*model (ISyn) = synapse(Vpre, Vpost, gSyn, ESyn, Vthresh)
	  equation ISyn = {(gSyn*(Vpost-ESyn)) when Vpre > Vthresh,
	  		 	  		0 otherwise}
end*/
