/*
Actively reduced leech HN model from Tobin et al, 2006
*/
function xinf(a, b, V) = 1/(1 + exp(a * (V + b)))
function taux(a, b, c, e, V) = c + e / (1 + exp(a * (V + b)))

model (Vm, Raxonal) = somaCompartment(Istim, Iaxonal, gK1, gK2, gKA, Rm, Eleak)

input Istim with {default = 0}
input Iaxonal with {default = 0}
input Rm with {default = 1.6e4}
input Eleak with {default = -50}

//************************************************
//Geometric Parameters
//************************************************
constant diameter = 23							//um
constant pi = 3.1415926535897932384626433832795

//************************************************
//Electrophysiologic Parameters
//************************************************
equation Ra = 250  					 			//Ohm*cm
constant Cm = 2.2 								//uF/cm^2

constant Raxonal = 0.4*1e-3					//GOhm

//Conductance densities
input gK1 with {default = 0}								//mS/cm^2
input gK2 with {default = 0}								//mS/cm^2
input gKA with {default = 0}								//mS/cm^2

//Reversal potentials
constant EK = -70									//mV

//************************************************
//State Declarations
//************************************************
state Vm = -45										//mV

state mK1 = 0.1									//unitless
state hK1 = 0.9									//unitless
state mK2 = 0.1									//unitless
state mKA = 0.1									//unitless
state hKA = 0.9									//unitless

//************************************************
//Model Equations
//************************************************
equations
   //Geometric and membrane parameters of spherical soma compartment
   SA = pi*(diameter/10000)^2					//cm^2

	//Calculate non-specific membrane properties
   Cmem = Cm*SA*1e3	                     //pF
   Gleak = (1e3/Rm)*SA*1e6	               //nS
	GK1 = gK1*SA*1e6								//nS
	GK2 = gK2*SA*1e6								//nS
	GKA = gKA*SA*1e6								//nS

	IK1 = GK1*mK1*hK1*(Vm - EK)				//pA
	IK2 = GK1*mK2*(Vm - EK)						//pA
	IKA = GK1*mKA*hKA*(Vm - EK)  				//pA
	Ileak = Gleak*(Vm - Eleak)					//pA

   ICmem = Istim - (Iaxonal + IK1 + IK2 + IKA + Ileak)

	//active current differential equations
	//Tau in seconds
	mK1' = (xinf(-0.143, 11, Vm) - mK1) / taux(.150, 6, .001,.011, Vm)
  	hK1' = (xinf(.111, 18, Vm) - hK1) / taux(-.143, 3, 0.5, 0.2, Vm)
  	mK2' =(xinf(-0.083, 10, Vm) - mK2) / taux(0.2, 25, 0.057, 0.043, Vm)
  	mKA' = (xinf(-.13, 34, Vm) - mKA) / taux(0.2, 20, 0.005, 0.011, Vm)
  	hKA' = (xinf(.16, 53, Vm) - hKA) / taux(-0.3, 45, 0.026, 0.0085, Vm)
   //Membrane potential differential equation, mV/sec   
   Vm' = (1/Cmem)*(ICmem)
end

//************************************************
//Solver Parameters
//************************************************
solver = forwardeuler
solver.dt = 1e-4
end