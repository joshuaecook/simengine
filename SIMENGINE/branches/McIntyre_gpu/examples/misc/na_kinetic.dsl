// Adapter from Kuo, Lee, Zhang, and Heckman, 2006, J. Physiol
// Essential role of the persistent sodium current in spike initiation during slowly
// rising inputs in mouse spinal neurones

model (INa,O) = na_kinetic(dur1,amp1,dur2,amp2,amp3,Oon,Ooff,m1Vh,m2Vh)

    constant mVs = 12		//	(mV)		: voltage sensitivity of activation
    constant aTau = 0.05	//		(ms)		: activation time constant
    constant gTau = 0.02	//		(ms)		: time constant for closed-open transition
    constant Con = 0.004	//		(/ms)	: rate constant for active closed to inactive closed
    constant Coff = 4.5		//		(/ms)	: rate constant for inactive closed to active closed
    constant tfac=4.7		//				: multiplication factor for temp for 22 to 37 C

    input dur1 with {default=10}
    input amp1 with {default=-70}
    input dur2 with {default=30}
    input amp2 with {default=-20}
    input amp3 with {default=-70}

    input Oon with {default= 3.936}		//		(/ms)	  : rate constant for active open to inactive open
    input Ooff with {default = 0.07208}	//		(/ms)	  : rate constant for inactive open to active open
    input m1Vh with {default =-26}		//		(mV)
    input m2Vh with {default=-96}		// (mV)


    equation V= {amp1 when (t < dur1 or t==dur1), 
		 amp2 when (t > dur1 and (t < dur1+dur2 or t==dur1+dur2)),
		 amp3 otherwise}

    equations
	alpha = 14*exp((V+27)/27.5)
	beta = 7.45*exp(-(V+27)/27.5)
	gamma = 1880
	delta = 32
//	Con = 0.026
//	Coff = 1.235
	a = (Oon/Con)^(1/4)
	b = (Ooff/Coff)^(1/4)
    end

    state C1 = 1/12
    state C2 = 1/12
    state C3 = 1/12
    state C4 = 1/12
    state C5 = 1/12
    state O = 1/12
    state I1 = 1/12
    state I2 = 1/12
    state I3 = 1/12
    state I4 = 1/12
    state I5 = 1/12
    state I6 = 1/12

    equations 
	C1' = beta*C2-4*alpha*C1+Coff*I1-Con*C1
	C2' = 2*beta*C3-3*alpha*C2+Coff/a*I2-Con*a*C2+4*alpha*C1-beta*C2
	C3' = 3*beta*C4-2*alpha*C3+Coff/a^2*I3-Con*a^2*C3+3*alpha*C2-2*beta*C3
	C4' = 4*beta*C5-alpha*C4+Coff/a^3*I4-Con*a^3*C4+2*alpha*C3-3*beta*C4
	C5' = delta*O-gamma*C5+Coff/a^4*I5-Con*a^4*C5+alpha*C4-4*beta*C5
	O' = Ooff*I6-Oon*O+gamma*C5-delta*O
	I1' = beta/a*I2-4*alpha*a*I1+Con*C1-Coff*I1
	I2' = 2*beta/a*I3-3*alpha*a*I2+Con*a*C2-Coff/a*I2+4*alpha*a*I1-beta/a*I2
	I3' = 3*beta/a*I4-2*alpha*a*I3+Con*a^2*C3-Coff/a^2*I3+3*alpha*a*I2-2*beta/a*I3
	I4' = 4*beta/a*I5-alpha*a*I4+Con*a^3*C4-Coff/a^3*I4+2*alpha*a*I3-3*beta/a*I4
	I5' = delta*I6-gamma*I5+Con*a^4*C5-Coff/a^4*I5+alpha*a*I4-4*beta/a*I5
	I6' = Oon*O-Ooff*I6+gamma*I5-delta*I6
    end

    constant GMax = 0.01487
    constant ENa = 50
    equation INa = GMax*(V-ENa)

    t {solver=cvode}

end
