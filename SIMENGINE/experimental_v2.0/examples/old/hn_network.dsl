/*  Leech heartbeat timing network model
    From Hill et al, 2001, J. Comp. Neuro
    Copyright 2007-2008 Simatra Modeling Technolgies
    Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)
*/

function xinf(a, b, V) = 1/(1 + exp(a * (V + b)))
function taux(a, b, c, e, V) = c + e / (1 + exp(a * (V + b)))
function cube (x) = x * x * x
function sqr (x) = x * x

//*****************************************Synapse**********************************************//
model (ISyn) = Synapse (Vpre, Vpost)
    constant Vthresh = -20   //mV
    parameter gSyn (0 to 100 by 1)  = 60   //nS   

    constant ESyn   = -62.5
    parameter tRise (0.001 to 0.01 by 0.001) = 0.002
    parameter tFall (0.001 to 0.1 by 0.001) = 0.011
    
    state fSyn (0 to 0.999 by 0.0001) = 0
    state MSyn (0 to 0.999 by 0.0001) = 0.1

    equations
      fSyn' = {(0.999-fSyn)/tRise when Vpre > Vthresh, -fSyn/tFall otherwise}
      MSyn' = (taux(-0.99, 40, 0.1, 0.9, Vpre) - MSyn)/0.2

      ISyn = gSyn * fSyn * MSyn * (Vpost - ESyn)
    end
end
//***********************************************************************************************//

//***************************************** HN 34 ***********************************************//
model (Vm) = HN34 (Vpre1, Vpre2)

//Membrane Capacitance
constant Cmem = 0.5 //pF

//Maximal conductances in nS
constant gNa = 200
constant gP = 7.0
constant gCaF = 5.0 
constant gCaS = 3.2
constant gK1 = 100
constant gK2 = 80
constant gKA = 80
parameter gh (0 to 16 by 0.1) = 4
parameter gleak (0 to 20 by 0.1) = 8

constant ENa = 45 //mV
constant ECa = 135 
constant EK = -70
constant Eh = -21
parameter Eleak (-80 to -50 by 0.1) = -65

//State Variable Declaration
visible state Vm (-81 to 136 by 0.000001) = -45
state hNa (0 to 0.999 by 0.00001 ) = 0.99 
state mP (0 to 0.999 by 0.00001 ) = 0.32 
state mCaS (0 to 0.999 by 0.00001 ) = 0.04 
state hCaS (0 to 0.999 by 0.0000001 ) = 0.78 
state mCaF (0 to 0.999 by 0.00001 ) = 0.02 
state hCaF (0 to 0.999 by 0.00001 ) = 0.74 
state mK1 (0 to 0.999 by 0.00001) = 0.03
state hK1 (0 to 0.999 by 0.00001) = 0.81
state mKA (0 to 0.999 by 0.00001) = 0.46
state hKA (0 to 0.999 by 0.00001) = 0.05
state mK2 (0 to 0.999 by 0.00001) = 0.16
state mh (0 to 0.999 by 0.0000001) = 0.08

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

submodel Synapse synapse1 //[dt, t, integrate, Vpre1, Vm]

 synapse1.Vpre = Vpre1
 synapse1.Vpost = Vm

submodel Synapse synapse2   //[dt, t, integrate, Vpre2, Vm]

 synapse2.Vpre = Vpre2
 synapse2.Vpost = Vm

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

  Vm' = 1/Cmem*(-INa-IP-ICaF-ICaS-IK1-IK2-IKA-Ih-Ileak-synapse1.ISyn-synapse2.ISyn)
end
end
//***********************************************************************************************//


//***************************************** HN 12 ***********************************************//
model (Vm) = HN12 (Vpre1, Vpre2)


//Membrane Capacitance
constant Cmem = 0.5 //nF

//Maximal conductances in nS
constant gNa = 250
constant gK1 = 150
constant gK2  = 75
parameter gleak (0 to 20 by 0.1) = 10

constant ENa = 45 //mV
constant EK = -70
parameter Eleak (-80 to 0 by 1) = -40

//State Variable Declaration
state Vm (-81 to 136 by 0.000001) = -45
state hNa (0 to 0.999 by 0.00001 ) = 0.99 
state mK1 (0 to 0.999 by 0.00001) = 0.03
state hK1 (0 to 0.999 by 0.00001) = 0.81
state mK2 (0 to 0.999 by 0.00001) = 0.16

equations
//Ionic Currents
mNa = xinf(-0.150, 29, Vm)
INa = gNa*cube(mNa)*hNa*(Vm - ENa)
IK1 = gK1*sqr(mK1)*hK1*(Vm - EK)
IK2 = gK2*sqr(mK2)*(Vm - EK)
Ileak = gleak*(Vm-Eleak)
//ISyn = 0
end

submodel Synapse synapse1

synapse1.Vpre = Vpre1
synapse1.Vpost = Vm

submodel Synapse synapse2

synapse2.Vpre = Vpre2
synapse2.Vpost = Vm

// Differential Equations

equations
  hNa' = (xinf(0.500, 30, Vm) - hNa) / (0.004 + 0.006/(1 + exp(0.5*(Vm + 28))) + 0.01/(cosh(0.3*(Vm + 27))))
  mK1' = (xinf(-0.143, 21, Vm) - mK1) / taux(.150, 16, .001,.011, Vm)
  hK1' = (xinf(.111, 28, Vm) - hK1) / taux(-.143, 13, 0.5, 0.2, Vm)
  mK2' =(xinf(-0.083, 20, Vm) - mK2) / taux(0.2, 35, 0.057, 0.043, Vm)
  Vm' = 1/Cmem*(-INa-IK1-IK2-Ileak-synapse1.ISyn-synapse2.ISyn)
end

end
//***********************************************************************************************//

//**********************************Define Elemental Oscillator**********************************//
model (Vm) = EO
   //Segment 3 Oscillator Definition
   submodel HN34 HNL3
   submodel HN34 HNR3

   //Segment 4 Oscillator Definition
   submodel HN34 HNL4
   submodel HN34 HNR4

   //Coordinating Interneurons
   submodel HN12 HNR12
   submodel HN12 HNL12

   //Connectivity
   HNL3.Vpre1 = HNR3.Vm
   HNL3.Vpre2 = HNL12.Vm

   HNR3.Vpre1 = HNL3.Vm
   HNR3.Vpre2 = HNR12.Vm

   HNL4.Vpre1 = HNR4.Vm
   HNL4.Vpre2 = HNL12.Vm

   HNR4.Vpre1 = HNL4.Vm
   HNR4.Vpre2 = HNR12.Vm

   HNR12.Vpre1 = HNR3.Vm
   HNR12.Vpre2 = HNR4.Vm
   
   HNL12.Vpre1 = HNL3.Vm
   HNL12.Vpre2 = HNL4.Vm

   output Vm = (HNL3.Vm, HNR3.Vm, HNL4.Vm, HNR4.Vm, HNR12.Vm, HNL12.Vm)
  solver = forwardeuler(0.0001)
  //solver.maxduration = 1000

//  setVisible |t|
//  setVisible |*Vm|
end

//compile (EO)
