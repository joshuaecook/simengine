/*  
 *   Lobster STG pacing neuron model
 *   Derived from Prinz et al, J Neurophysiol, December 2003
 *   Copyright 2008 Simatra Modeling Technolgies
 */

function xinf(a, b, V) = 1/(1 + exp((V + a)/b))
function taux(a, b, c, e, V) = c + e / (1 + exp((V + a)/b))

//*****************************************STG***********************************************//
model (Vm, metrics)=pd_events(gNa, gCaT, gCaS, gA, gKCa, gKd, gh, gleak)

// Define exponential euler iterator
iterator t_expeuler with {continuous, solver=exponentialeuler{dt=0.05}}
t{continuous, solver=forwardeuler{dt=0.05}}

//Membrane properties
constant Amem = 0.628e-3 //cm^2
constant Cmem = 0.000628 //nF 

//Maximal conductances in mS/cm^2
input gNa with {default=100}
input gCaT with {default=0}
input gCaS with {default=10}
input gA with {default=40}
input gKCa with {default=25}
input gKd with {default=75}
input gh with {default=0.02}
input gleak with {default=0.03}

constant ENa = 50 //mV
constant EK = -80
constant Eh = -20
constant Eleak = -50

//State Variable Declaration
state Vm = -57.5 with {iter=t_expeuler} //mV
state mNa = 0.002 //by was 0.00000006
state hNa = 0.856
state mCaT = 0.013
state hCaT = 0.991
state mCaS = 0.042
state hCaS = 0.396
state mA = 0.027
state hA = 0.571
state mKCa = 0.027
state mKd = 0.02
state mh = 0.031
state Caconc = 0.05 with {iter=t_expeuler} //uM

equations
    // Helper functions
    xinf(a, b, V) => 1/(1 + exp((V + a)/b))
    taux(a, b, c, e, V) => c + e / (1 + exp((V + a)/b))

   //store previous values of Vm
   ECa = 29.55*ln(3000/Caconc)

   // deal with mKCa going below zero
   mKCa = 0 when mKCa < 0
   
   //Ionic Currents in uA
   INa = gNa*mNa^3*hNa*(Vm - ENa)*Amem
   ICaT = gCaT*mCaT^3*hCaT*(Vm - ECa)*Amem
   ICaS = gCaS*mCaS^3*hCaS*(Vm - ECa)*Amem
   IA = gA*mA^3*hA*(Vm-EK)*Amem
   IKCa = gKCa*mKCa^4*(Vm-EK)*Amem
   IKd = gKd*mKd^4*(Vm-EK)*Amem
   Ih = gh*mh*(Vm - Eh)*Amem
   Ileak = gleak*(Vm-Eleak)*Amem

   // Differential Equations
   // Time constants are in msec
   Caconc' = (1/200)*(-14960*(ICaT + ICaS) - Caconc + 0.05) //uM for CaConc
   mNa' = (xinf(25.5, -5.29, Vm) - mNa)/(taux(120, -25, 2.64, -2.52, Vm))
   hNa' = (xinf(48.9, 5.18, Vm) - hNa)/(taux(62.9, -10, 0, 1.34, Vm)*taux(34.9, 3.6, 1.5, 1, Vm))
   mCaT' = (xinf(27.1, -7.2, Vm) - mCaT)/(taux(68.1, -20.5, 43.4, -42.6, Vm))
   hCaT' = (xinf(32.1, 5.5, Vm) - hCaT)/(taux(55, -16.9, 210, -179.6, Vm))
   mCaS' = (xinf(33, -8.1, Vm) - mCaS)/(2.8 + 14/(exp((Vm +27)/10) + exp((Vm + 70)/-13)))
   hCaS' = (xinf(60, 6.2, Vm) - hCaS)/(120 + 300/(exp((Vm + 55)/9) + exp((Vm+65)/-16)))
   mA' = (xinf(27.2, -8.7, Vm) - mA)/(taux(32.9, -15.2, 23.2, -20.8, Vm))
   hA' = (xinf(56.9, 4.9, Vm) - hA)/(taux(38.9, -26.5, 77.2, -58.4, Vm))
   temp = ((Caconc/1)/((Caconc/1) + 3))*xinf(28.3, -12.6, Vm)
   mKCa' = ({0 when temp < 0, temp otherwise} - mKCa)/(taux(46, -22.7, 180.6, -150.2, Vm))	      
   mKd' = (xinf(12.3, -11.8, Vm) - mKd)/(taux(28.3, -19.2, 14.4, -12.8, Vm))
   mh' = (xinf(75, 5.5, Vm) - mh)/(2/(exp((Vm + 169.7)/-11.6) + exp((Vm - 26.7)/14.3)))
   Vm' = (1/Cmem)*(-INa-ICaT-ICaS-IA-IKCa-IKd-Ih-Ileak)
end

// System Metrics
state last_spike_height = 0
state last_spike_time = 0 
state last_spike_area = 0

equations
   last_spike_area = {0 when (Vm[t_expeuler[-1]] < -40 and Vm[t_expeuler[-2]] >= -40),
   		      last_spike_area + (Vm[t_expeuler[-1]]+40)*(0.05/1000) when (Vm[t_expeuler[-1]] >= -40 and Vm[t_expeuler[-1]] <=-15),
   		      last_spike_area + 20*(0.05/1000) when (Vm[t_expeuler[-1]] > -15),
                      last_spike_area otherwise}

  last_spike_time = {t when (Vm < Vm[t_expeuler[-1]] and Vm[t_expeuler[-1]] > Vm[t_expeuler[-2]]),
                     last_spike_time otherwise}
  last_spike_height = Vm[t_expeuler[-1]] when (Vm < Vm[t_expeuler[-1]] and Vm[t_expeuler[-1]] > Vm[t_expeuler[-2]])

  output_data = (Vm < -40 and Vm[t_expeuler[-1]] >= -40 and t > 10000)
end

//time is in msec
solver = ode45

output Vm with {condition=t>10000}
output metrics[t] = (last_spike_time, last_spike_height, last_spike_area) with {condition=output_data}

end
