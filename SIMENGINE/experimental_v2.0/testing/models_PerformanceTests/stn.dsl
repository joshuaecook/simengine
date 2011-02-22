model (Vm, spike) = stn(inhibitoryInput) 
    input inhibitoryInput with {default = 0}

    constant Cm = 1 
    
    //maximal conductances
    constant gna = 37
    constant gsyn = 1
    constant gk = 45 
    constant gl = 2.25 
    constant gt = 0.5     
    constant gca = 0.5 
    constant gahp = 9 

    //reversal potentials
    constant Ena = 55 
    constant Ek = -80 
    constant Eca = 140 
    constant El = -60 
    constant Esyn = -85

    //time constants
    constant taur = 30
   
    //other model parameters
    constant k1 = 15
    constant S3 = 0
    equation Iapp = 25
    constant kca = 22.5 
    
    constant tfallI = 5.1
    constant triseI = 1.1

    state Vm = -62
    state Ca = 0.21
    state n = 0.11
    state h = 0.08
    state r = 0.15
    state mSynI = 0
    state hSynI = 0

    equations
	//threshold crossing detection
	spike = (Vm > -25 and Vm[t[-1]] <= -25)

	ninf = 1/(1 + exp(-(Vm + 32)/8))
	taun = 1 + 100/(1 + exp(-(Vm+80)/-26))

	hinf = 1/(1 + exp((Vm+39)/3.1))
	tauh = 1 + 500/(1 + exp(-(Vm+57)/-3))

	rinf = 1/(1 + exp((Vm + 67)/2))

	m = 1/(1 + exp(-(Vm+30)/15))
	a = 1/(1 + exp(-(Vm+63)/7.8))
		
	Il= gl*(Vm-El)
	Ik= gk*(n^4)*(Vm-Ek)
	Ina= gna*(m^3)*h*(Vm-Ena)
	It= gt*(a^3)*r*(Vm-Eca)
	Ica= gca*(S3^2)*(Vm-Eca)
	Iahp= gahp*(Vm-Ek)*(Ca/(Ca+k1))
	IsynE= gsyn*(mSynI-hSynI)*(Vm-Esyn)

	Vm'=1/Cm*(-Il-Ik-Ina-It-Ica-Iahp-IsynE+Iapp)
				
        Ca'=3.75*10^-5*(-Ica-It-kca*Ca)
	n'=0.75*(ninf-n)/taun
	h'=0.75*(hinf-h)/tauh
	r'=0.5*(rinf-r)/taur

	//Excitatory Synapse State Variables
	mSynI'= -mSynI/tfallI
	mSynI = mSynI+inhibitoryInput when inhibitoryInput > 0
	hSynI'= -hSynI/triseI 
        hSynI = hSynI+inhibitoryInput when inhibitoryInput > 0
    end

output spike with {condition = spike > 0}

solver = cvode
solver.dt = 2e-1
//solver = ode23
end
