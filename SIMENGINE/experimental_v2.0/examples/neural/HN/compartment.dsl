/*
Actively reduced leech HN model from Tobin et al, 2006
*/
function xinf(a, b, V) = 1/(1 + exp(a * (V + b)))
function taux(a, b, c, e, V) = c + e / (1 + exp(a * (V + b)))

model (Vm, Raxonal) = compartment(Iaxonal, length, diameter, gNa, gK1, gK2, gKA, gh, gP, gCaF, gCaS, Rm, Eleak)

//input Gleak with {default = 0}
//input Eleak with {default = -65}

input Iaxonal with {default = 0}			//uA

input length with {default = 10}			//um
input diameter with {default = 10}		//um

input gNa with {default = 0}				//mS/cm^2
input gh with {default = 0}				//mS/cm^2
input gP with {default = 0}				//mS/cm^2
input gCaF with {default = 0}				//mS/cm^2
input gCaS with {default = 0}				//mS/cm^2
input gK1 with {default = 0}				//mS/cm^2
input gK2 with {default = 0}				//mS/cm^2
input gKA with {default = 0}				//mS/cm^2

input Rm with {default = 1.6e4}
input Eleak with {default = -50}
//************************************************
//Geometric Parameters
//************************************************
constant pi = 3.1415926535897932384626433832795

//************************************************
//Electrophysiologic Parameters
//************************************************
constant Ra = 250  					 			//Ohm*cm
constant Cm = 2.2 								//uF/cm^2

equation Raxonal = (4*(length/10000)*Ra)/(pi*(diameter/10000)^2)/1e9						//GOhm <-> nS

//Reversal potentials
constant EK = -70									//mV
constant ENa = 45									//mV
constant ECa = 135								//mV
constant Eh = -21									//mV

//************************************************
//State Declarations
//************************************************
state Vm = -45										//mV

equation	mNa = xinf(-0.150, 31, Vm)			//unitless 
state hNa = 0.9 									//unitless
state mP = 0.1 									//unitless
state mCaS = 0.1	 								//unitless
state hCaS = 0.9  								//unitless
state mCaF = 0.1  								//unitless
state hCaF = 0.9  								//unitless
state mK1 = 0.1									//unitless
state hK1 = 0.9									//unitless
state mK2 = 0.1									//unitless
state mKA = 0.1									//unitless
state hKA = 0.9									//unitless
state mh = 0.1										//unitless
//************************************************
//Model Equations
//************************************************
equations
   //Geometric and membrane parameters of spherical soma compartment
   SA = pi*(length/10000)*(diameter/10000)					//cm^2

	//Calculate non-specific membrane properties
   Cmem = Cm*SA*1e3	                        //nF
   
	//Conductance densities
	GNa = gNa*SA*1e6									//nS
	GP = gP*SA*1e6										//nS
	GCaS = gCaS*SA*1e6								//nS
	GCaF = gCaF*SA*1e6								//nS
	GK1 = gK1*SA*1e6									//nS
	GK2 = gK2*SA*1e6									//nS
	GKA = gKA*SA*1e6									//nS
	Gh = gh*SA*1e6										//nS
	Gleak = (1e3/Rm)*SA*1e6	                  //nS

	INa = GNa*mNa^3*hNa*(Vm - ENa)			//pA
	IP = GP*mP*(Vm - ENa)						//pA
	ICaF = GCaF*mCaF^2*hCaF*(Vm - ECa)		//pA
	ICaS = GCaS*mCaS^2*hCaS*(Vm - ECa)		//pA
	IK1 = GK1*mK1^2*hK1*(Vm - EK)				//pA
	IK2 = GK2*mK2^2*(Vm - EK)					//pA
	IKA = GKA*mKA^2*hKA*(Vm - EK)				//pA
	Ih = Gh*mh^2*(Vm - Eh)						//pA
	Ileak = Gleak*(Vm-Eleak)					//pA

   ICmem = -(Iaxonal + INa + IP + ICaF + ICaS+ IK1 + IK2 + IKA + Ih + Ileak)

	//active current differential equations
	hNa' = (xinf(0.500, 32, Vm) - hNa) / (0.004 + 0.006/(1 + exp(0.5*(Vm + 30))) + 0.01/(cosh(0.3*(Vm + 29))))	//tau in seconds
	mP' = (xinf(-0.120, 39, Vm) - mP) / taux(0.4, 57, 0.01, 0.2, Vm)															//tau in seconds
  	mCaF' = (xinf(-0.6, 46.7, Vm) - mCaF) / (0.011 + 0.024/cosh(0.3*(Vm + 46.7)))											//tau in seconds
  	hCaF' = (xinf(0.35, 55.5, Vm) - hCaF) / taux(0.27, 55, 0.06, 0.31, Vm)													//tau in seconds
  	mCaS' = (xinf(-0.42, 47.2, Vm) - mCaS) / taux(-0.4, 48.7, 0.005, 0.134, Vm)											//tau in seconds
  	hCaS' = (xinf(0.36, 55, Vm) - hCaS) / taux(-0.25, 43, 0.2, 5.25, Vm)														//tau in seconds
  	mK1' = (xinf(-0.143, 11, Vm) - mK1) / taux(.150, 6, .001,.011, Vm)														//tau in seconds
  	hK1' = (xinf(.111, 18, Vm) - hK1) / taux(-.143, 3, 0.5, 0.2, Vm)															//tau in seconds
  	mK2' =(xinf(-0.083, 10, Vm) - mK2) / taux(0.2, 25, 0.057, 0.043, Vm)														//tau in seconds
  	mKA' = (xinf(-.13, 34, Vm) - mKA) / taux(0.2, 20, 0.005, 0.011, Vm)														//tau in seconds
  	hKA' = (xinf(.16, 53, Vm) - hKA) / taux(-0.3, 45, 0.026, 0.0085, Vm)														//tau in seconds
  	mh' = (1/(1 + 2*exp(.180*(Vm + 47)) + exp(0.5*(Vm + 47))) - mh) / taux(-0.1, 73, 0.7, 1.7, Vm)					//tau in seconds

   //Membrane potential differential equation   
   Vm' = (1/Cmem)*(ICmem)  //(pA/nF) -> mV/s
end

//************************************************
//Solver Parameters
//************************************************
solver = forwardeuler
solver.dt = 1e-4
end