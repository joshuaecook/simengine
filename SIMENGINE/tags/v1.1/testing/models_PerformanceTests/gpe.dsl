model (Vm, spike, tmp) = gpe(excitatoryInput, inhibitoryInput) 
    input excitatoryInput with {default = 0}
    input inhibitoryInput with {default = 0}

    constant Cm = 1 
    
    //maximal conductances
    constant gna = 120
    constant gsynE = 0.3
    constant gsynI = 1.0
    constant gk = 30 
    constant gl = 0.1 
    constant gt = 0.5     
    constant gca = 0.15 
    constant gahp = 30 

    //reversal potentials
    constant Ena = 55 
    constant Ek = -80 
    constant Eca = 120 
    constant El = -55 
    constant EsynE = 0
    constant EsynI = -85

    //time constants
    constant taur = 30
   
    //other model parameters
    constant pd = 3 
    constant k1 = 30
    constant S3 = 0
    equation Iappgpe=20-pd*5
    constant kca = 15 
    
    constant tfallI = 5.1
    constant triseI = 1.1

    constant tfallE = 2.0
    constant triseE = 0.5

    //definition of model states
    state Vm = -62
    state Ca = 0.21
    state n = 0.11
    state h = 0.08
    state r = 0.15
    state mSynE = 0
    state hSynE = 0
    state mSynI = 0
    state hSynI = 0

    equations
	//threshold crossing detection
	spike = (Vm > -25 and Vm[t[-1]] <= -25)

	ninf = 1/(1 + exp(-(Vm + 50)/14))
	taun = 0.05 + 0.27/(1 + exp(-(Vm+40)/-12))
	hinf = 1/(1 + exp((Vm+58)/12))
	tauh = 0.05 + 0.27/(1 + exp(-(Vm+40)/-12))
	rinf = 1/(1 + exp((Vm + 70)/2))
	m = 1/(1 + exp(-(Vm+37)/10))
	a = 1/(1 + exp(-(Vm+57)/2))
		
	Il= gl*(Vm-El)
	Ik= gk*(n^4)*(Vm-Ek)
	Ina= gna*(m^3)*h*(Vm-Ena)
	It= gt*(a^3)*r*(Vm-Eca)
	Ica= gca*(S3^2)*(Vm-Eca)
	Iahp= gahp*(Vm-Ek)*(Ca/(Ca+k1))
	IsynE = gsynE*(mSynE-hSynE)*(Vm-EsynE)
	IsynI = gsynI*(mSynI-hSynI)*(Vm-EsynI)

	Vm'=1/Cm*(-Il-Ik-Ina-It-Ica-Iahp-IsynE-IsynI+Iappgpe)
				
        Ca'=1*10^-4*(-Ica-It-kca*Ca)
	n'=0.1*(ninf-n)/taun
	h'=0.05*(hinf-h)/tauh
	r'=(rinf-r)/taur

	//Excitatory Synapse State Variables
	mSynE'= -mSynE/tfallE
	mSynE = mSynE+excitatoryInput when excitatoryInput > 0
	hSynE'= -hSynE/triseE 
        hSynE = hSynE+excitatoryInput when excitatoryInput > 0

	//Inhibitory Synapse State Variable
	mSynI'= -mSynI/tfallI 
	mSynI = mSynI+inhibitoryInput when inhibitoryInput > 0
	hSynI'= -hSynI/triseI 
        hSynI = hSynI+inhibitoryInput when inhibitoryInput > 0
    end

output spike with {condition = spike > 0}
output tmp = IsynE
solver = cvode
solver.dt = 2e-1
//solver = ode23
end
