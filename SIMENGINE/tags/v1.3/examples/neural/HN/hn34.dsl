/*  
    Leech heartbeat timing network model (hn34 model)
    From Hill et al, 2001, J. Comp. Neuro
    Copyright 2007-2008 Simatra Modeling Technolgies
    Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)
*/

function xinf(a, b, V) = 1/(1 + exp(a * (V + b)))
function taux(a, b, c, e, V) = c + e / (1 + exp(a * (V + b)))
function cube (x) = x * x * x
function sqr (x) = x * x

//***************************************** HN 34 ***********************************************//
model (Vm) = hn34(gh, gleak, Eleak, ISyn, IStim)
	//Membrane Capacitance
	constant Cmem = 0.499 //pF

	//Maximal conductances in nS
	constant gNa = 200
	constant gP = 7.0
	constant gCaF = 5.0 
	constant gCaS = 3.2
	constant gK1 = 100
	constant gK2 = 80
	constant gKA = 80
	input gh with {default = 4}
	input gleak with {default = 8}

	constant ENa = 45 //mV
	constant ECa = 135 
	constant EK = -70
	constant Eh = -21
	input Eleak with {default = -65}

	input ISyn with {default = 0}
	input IStim with {default = 0}

	//State Variable Declaration
	state Vm = -45
	state hNa = 0.99 
	state mP = 0.32 
	state mCaS = 0.04 
	state hCaS = 0.78 
	state mCaF = 0.02 
	state hCaF = 0.74 
	state mK1 = 0.03
	state hK1 = 0.81
	state mKA = 0.46
	state hKA = 0.05
	state mK2 = 0.16
	state mh = 0.08

	//Ionic Currents
	equations
		mNa = xinf(-0.150, 29, Vm)
		INa = gNa*cube(mNa)*hNa*(Vm - ENa)
		IP = gP*mP*(Vm - ENa)
		ICaF = gCaF*sqr(mCaF)*hCaF*(Vm - ECa)
		ICaS = gCaS*sqr(mCaS)*hCaS*(Vm - ECa)
		IK1 = gK1*sqr(mK1)*hK1*(Vm - EK)
		IK2 = gK2*sqr(mK2)*(Vm - EK)
		IKA = gKA*sqr(mKA)*hKA*(Vm - EK)
		Ih = gh*sqr(mh)*(Vm - Eh)
		Ileak = gleak*(Vm-Eleak)
	end

// Differential Equations
	equations
	  	hNa' = (xinf(0.500, 30, Vm) - hNa) / (0.004 + 0.006/(1 + exp(0.5*(Vm + 28))) + 0.01/(cosh(0.3*(Vm + 27))))
		mP' = (xinf(-0.120, 39, Vm) - mP) / taux(0.4, 57, 0.01, 0.2, Vm)
  		mCaF' = (xinf(-0.6, 46.7, Vm) - mCaF) / (0.011 + 0.024/cosh(0.3*(Vm + 46.7)))
  		hCaF' = (xinf(0.35, 55.5, Vm) - hCaF) / taux(0.27, 55, 0.06, 0.31, Vm)
  		mCaS' = (xinf(-0.42, 47.2, Vm) - mCaS) / taux(-0.4, 48.7, 0.005, 0.134, Vm)
  		hCaS' = (xinf(0.36, 55, Vm) - hCaS) / taux(-0.25, 43, 0.2, 5.25, Vm)
  		mK1' = (xinf(-0.143, 21, Vm) - mK1) / taux(.150, 16, .001,.011, Vm)
  		hK1' = (xinf(.111, 28, Vm) - hK1) / taux(-.143, 13, 0.5, 0.2, Vm)
  		mK2' =(xinf(-0.083, 20, Vm) - mK2) / taux(0.2, 35, 0.057, 0.043, Vm)
  		mKA' = (xinf(-.13, 44, Vm) - mKA) / taux(0.2, 30, 0.005, 0.011, Vm)
  		hKA' = (xinf(.16, 63, Vm) - hKA) / taux(-0.3, 55, 0.026, 0.0085, Vm)
  		mh' = (1/(1 + 2*exp(.180*(Vm + 47)) + exp(0.5*(Vm + 47))) - mh) / taux(-0.1, 73, 0.7, 1.7, Vm)
  		Vm' = 1/Cmem*(-INa-IP-ICaF-ICaS-IK1-IK2-IKA-Ih-Ileak+IStim-ISyn)
	end

solver = forwardeuler
solver.dt = 1e-4
end
