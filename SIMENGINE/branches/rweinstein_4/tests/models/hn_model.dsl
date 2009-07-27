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
model Synapse
    constant Vthresh = -20   //mV
    parameter gSyn (0 to 100 by 1)  = 60   //nS   

    constant ESyn   = -62.5
    parameter tRise (0.001 to 0.01 by 0.001) = 0.002
    parameter tFall (0.001 to 0.1 by 0.001) = 0.011
    
    state fSyn (0 to 0.999 by 0.0001) = 0
    state MSyn (0 to 0.999 by 0.0001) = 0.1

    equations
      Vpre = 0
      Vpost = 0
      fSyn' = {(0.999-fSyn)/tRise when Vpre > Vthresh, -fSyn/tFall otherwise}
      MSyn' = (taux(-0.99, 40, 0.1, 0.9, Vpre) - MSyn)/0.2

      ISyn = gSyn * fSyn * MSyn * (Vpost - ESyn)
    end
end
//***********************************************************************************************//

//***************************************** HN 34 ***********************************************//
model HN34// (DYNAMIC dt, DYNAMIC t, constant integrate, DYNAMIC Vpre1, DYNAMIC Vpre2)

//presynaptic inputs (can be overloaded to connect)
equation Vpre1 = 0
equation Vpre2 = 0

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
//ISyn = {10 * (Vm + 65) WHEN Vpre1 > -20,
//	0 OTHERWISE}
end

submodel synapse1 = Synapse.new() //[dt, t, integrate, Vpre1, Vm]

equation synapse1.Vpre = Vpre1
equation synapse1.Vpost = Vm


submodel synapse2 = Synapse.new()   //[dt, t, integrate, Vpre2, Vm]

equation synapse2.Vpre = Vpre2
equation synapse2.Vpost = Vm


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
model HN12 //(DYNAMIC dt, DYNAMIC t, constant integrate, DYNAMIC Vpre1, DYNAMIC Vpre2)


//presynaptic inputs (can be overloaded to connect)
equation Vpre1 = 0
equation Vpre2 = 0


//Membrane Capacitance
constant Cmem = 0.5 //pF

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

submodel synapse1 = Synapse.new()//[dt, t, integrate, Vpre1, Vm]

equation synapse1.Vpre = Vpre1
equation synapse1.Vpost = Vm

submodel synapse2 = Synapse.new()//[dt, t, integrate, Vpre2, Vm]

equation synapse2.Vpre = Vpre2
equation synapse2.Vpost = Vm

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
//DEFSYSTEM EO (DYNAMIC dt, DYNAMIC t, constant integrate)
model EO
   //Segment 3 Oscillator Definition
   submodel HNL3 = HN34.new()
   submodel HNR3 = HN34.new()

   //Segment 4 Oscillator Definition
   submodel HNL4 = HN34.new()
   submodel HNR4 = HN34.new()

   //Coordinating Interneurons
   submodel HNR12 = HN12.new()
   submodel HNL12 = HN12.new()

   //Connectivity
   equation HNL3.Vpre1 = HNR3.Vm
   equation HNL3.Vpre2 = HNL12.Vm

   equation HNR3.Vpre1 = HNL3.Vm
   equation HNR3.Vpre2 = HNR12.Vm

   equation HNL4.Vpre1 = HNR4.Vm
   equation HNL4.Vpre2 = HNL12.Vm

   equation HNR4.Vpre1 = HNL4.Vm
   equation HNR4.Vpre2 = HNR12.Vm

   equation HNR12.Vpre1 = HNR3.Vm
   equation HNR12.Vpre2 = HNR4.Vm
   
   equation HNL12.Vpre1 = HNL3.Vm
   equation HNL12.Vpre2 = HNL4.Vm

/* THE OLD DEFINITION:
   state VmR3 (-81 to 136 by 0.0001) = 0
   state VmL3 (-81 to 136 by 0.0001) = 0
   state VmR4 (-81 to 136 by 0.0001) = 0
   state VmL4 (-81 to 136 by 0.0001) = 0   
   state VmR12 (-81 to 136 by 0.0001) = 0
   state VmL12 (-81 to 136 by 0.0001) = 0

   //Segment 3 Oscillator Definition
   SYSTEM HNL3 = new HN34[dt, t, integrate, VmR3 , VmL12]
   SYSTEM HNR3 = new HN34[dt, t, integrate, VmL3 , VmR12]
   
   //Segment 4 Oscillator Definition
   SYSTEM HNL4 = new HN34[dt, t, integrate, VmR4, VmL12 ]
   SYSTEM HNR4 = new HN34[dt, t, integrate, VmL4, VmR12 ]

   //Coordinating Interneurons
   SYSTEM HNR12 = new HN12[dt, t, integrate, VmR3, VmR4 ]
   SYSTEM HNL12 = new HN12[dt, t, integrate, VmL3, VmL4 ]

   VmR3 = HNR3.Vm
   VmL3 = HNL3.Vm

   VmR4 = HNR4.Vm
   VmL4 = HNL4.Vm

   VmR12 = HNR12.Vm
   VmL12 = HNL12.Vm
*/

  t.setPrecision (Range.new(0, 1000, 0.0005))

  solver=forwardeuler(0.001)

  settings.hardware.iobitwidth.setValue(32)

  setVisible |t|
  setVisible |*Vm|
end


//ENDSYSTEM EO
//***********************************************************************************************//
/*
MAIN
  FUN euler_integrate (dt, t, state, eq) =
    state + dt * eq(t)

  FUN integrate (dt, t, state, eq) = 
    euler_integrate (dt, t, state, eq)
  
  state t (0 to 600 by 0.00001) = 0
  constant dt = 0.0001
  t=t+dt
  
  SYSTEM network = new EO [dt, t, integrate]

  OUTPUT "t", "network.V*", "network.HNL3.ISyn1"  
  OUTPUTRATE 20.0 
ENDMAIN
*/