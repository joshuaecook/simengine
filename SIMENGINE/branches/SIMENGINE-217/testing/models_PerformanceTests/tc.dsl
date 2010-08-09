model (Vm, spike) = tc(inhibitoryInput) 
    input inhibitoryInput with {default = 0}

    constant Cm = 1 
    
    //maximal conductances
    constant gna = 3
    constant gsynI = .08
    constant gk = 5 
    constant gl = 0.05 
    constant gt = 5     
    constant gca = 0.15 
    constant gahp = 30 
    constant gk2p1 = 10

    //reversal potentials
    constant Ena = 50 
    constant Ek = -90 
    constant Eca = 0 
    constant El = -70 
    constant EsynI = -85

    //time constants
   
    //other model parameters
    constant pd = 3 
    constant k1 = 30
    constant S3 = 0
    equation Iappgpe=20-pd*5
    constant kca = 15 
    
    constant tfallI = 5.1
    constant triseI = 1.1

    //definition of model states
    state Vm = -62
    state n = 0.11
    state h = 0.08
    state r = 0.15
    state mSynI = 0
    state hSynI = 0
 
    equations
	//threshold crossing detection
	spike = (Vm > -25 and Vm[t[-1]] <= -25)

        pinf = 1/(1+exp(-(Vm+60)/6.2))

	ninf = 1/(1 + exp(-(Vm + 50)/14))
	taun = 0.05 + 0.27/(1 + exp(-(Vm+40)/-12))
	
	hinf = 1/(1 + exp((Vm+41)/4))
	tauh = 1/((0.128*exp(-(Vm+46)/18)) + 4/(1 + exp(-(Vm+23)/5)))
	
	rinf = 1/(1 + exp(-(Vm + 84)/4))
        taur = 0.4*(28 + exp(-(Vm+25)/10.5))

	m = 1/(1 + exp(-(Vm+37)/7))
	a = 1/(1 + exp(-(Vm+57)/2))
	
	sinf = 0.26+(0.74/(1+exp((Vm+25)/12)))
	//taus = 800+500./(exp((V-50)./20)+exp((50-Vm)./20))

	Il= gl*(Vm-El)
	Ik= gk*((0.75*(1-h))^4)*(Vm - Ek)
	Ina= gna*(m^3)*h*(Vm-Ena)
	It= gt*pinf^2*r*(Vm-Eca)

	IsynI = gsynI*(mSynI-hSynI)*(Vm-EsynI)

	Vm'=1/Cm*(-Il-Ik-Ina-It-IsynI)
				
 	n'=0.1*(ninf-n)/taun
	h'=0.05*(hinf-h)/tauh
	r'=(rinf-r)/taur

	//Inhibitory Synapse State Variable
	mSynI'= -mSynI/tfallI 
	mSynI = mSynI+inhibitoryInput when inhibitoryInput > 0
	hSynI'= -hSynI/triseI 
        hSynI = hSynI+inhibitoryInput when inhibitoryInput > 0
    end

output spike with {condition = spike > 0}
//output tmp = IsynI
solver = cvode
solver.dt = 2e-1
//solver = ode23
end
