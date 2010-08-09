/*  Lobster STG pacing neuron model
    Derived from Prinz et al, J Neurophysiol, December 2003
    Copyright 2008 Simatra Modeling Technolgies
*/
settings.optimization.lutmaxwidth.value = 32

function xinf(a, b, V) = 1/(1 + exp((V + a)/b))
function taux(a, b, c, e, V) = c + e / (1 + exp((V + a)/b))
function cube (x) = x * x * x
function sqr (x) = x * x

//*****************************************STG***********************************************//
model stg

//Membrane properties
constant Amem = 0.628e-3 //cm^2
constant Cmem = 0.000628 //nF 

//Maximal conductances in mS/cm^2
parameter gNa (0 to 500 by 100) = 100
parameter gCaT (0 to 12.5 by 2.5) = 0
parameter gCaS (0 to 10 by 2) = 10
parameter gA (0 to 50 by 10) = 40
parameter gKCa (0 to 25 by 5) = 25
parameter gKd (0 to 125 by 25) = 75
parameter gh (0 to 0.05 by 0.01) = 0.02
parameter gleak (0 to 0.05 by 0.01) = 0.03
parameter Vclamp (-100 to 100 by 1) = -60
parameter clampOn (0 to 1 by 1) = 0

constant ENa = 50 //mV
constant EK = -80
constant Eh = -20
constant Eleak = -50

//State Variable Declaration
state V (-127 to 127 by 1.2e-7) = -57.5 //mV
state mNa (0 to 1.0 by 4.66e-10) = 0.002 //by was 0.00000006
state hNa (0 to 1.0 by 4.66e-10 ) = 0.856
state mCaT (0 to 1.000 by 4.66e-10 ) = 0.013
state hCaT (0 to 1.000 by 4.66e-10 ) = 0.991
state mCaS (0 to 1.000 by 4.66e-10 ) = 0.042
state hCaS (0 to 1.000 by 4.66e-10 ) = 0.396
state mA (0 to 1.000 by 4.66e-10 ) = 0.027
state hA (0 to 1.000 by 4.66e-10 ) = 0.571
state mKCa (-1.000 to 1.000 by 4.66e-10 ) = 0.027
state mKd (0 to 1.000 by 4.66e-10 ) = 0.02
state mh (0 to 1.000 by 4.66e-10 ) = 0.031
state Caconc (0.0001 to 3000 by 2.38e-7) = 0.05 //uM

equations
   Vm = V
   //store previous values of Vm
   ECa = 29.55*ln(3000/Caconc)

   // deal with mKCa going below zero
   saturated_mKCa = {0 when mKCa < 0,
                     mKCa otherwise}
   
   //Ionic Currents in uA
   INa = gNa*cube(mNa)*hNa*(Vm - ENa)*Amem
   ICaT = gCaT*cube(mCaT)*hCaT*(Vm - ECa)*Amem
   ICaS = gCaS*cube(mCaS)*hCaS*(Vm - ECa)*Amem
   IA = gA*cube(mA)*hA*(Vm-EK)*Amem
   IKCa = gKCa*sqr(saturated_mKCa)*sqr(saturated_mKCa)*(Vm-EK)*Amem
   IKd = gKd*sqr(mKd)*sqr(mKd)*(Vm-EK)*Amem
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
   mKCa' = ({0 when temp < 0, temp otherwise} - saturated_mKCa)/(taux(46, -22.7, 180.6, -150.2, Vm))	      
   //mKCa' = (((Caconc/1)/((Caconc/1) + 3))*xinf(28.3, -12.6, Vm) - saturated_mKCa)/(taux(46, -22.7, 180.6, -150.2, Vm))	      
   mKd' = (xinf(12.3, -11.8, Vm) - mKd)/(taux(28.3, -19.2, 14.4, -12.8, Vm))
   mh' = (xinf(75, 5.5, Vm) - mh)/(2/(exp((Vm + 169.7)/-11.6) + exp((Vm - 26.7)/14.3)))
   V' = (1/Cmem)*(-INa-ICaT-ICaS-IA-IKCa-IKd-Ih-Ileak)
end

//System Metrics

state last_spike_height (-127 to 127 by 1.2e-7) = 0
state last_spike_time (0 to 30000 by 0.05) = 0 
state last_spike_area (0 to 10 by 0.001) = 0

state last_Vm (-127 to 127 by 1.2e-7) = 0
state before_last_Vm (-127 to 127 by 1.2e-7) = 0

equations
  before_last_Vm_[0] = last_Vm
  last_Vm_[0] = Vm
  last_spike_area_[0] = {0 when (last_Vm < -40 and Vm >= -40),
  		     last_spike_area + (last_Vm+40)*(0.05/1000) when (last_Vm >= -40 and last_Vm <=-15),
		     last_spike_area + 20*(0.05/1000) when (last_Vm > -15),
                     last_spike_area otherwise}
  last_spike_time_[0] = {t when (Vm < last_Vm and last_Vm > before_last_Vm),
                     last_spike_time otherwise}
  last_spike_height_[0] = {last_Vm when (Vm < last_Vm and last_Vm > before_last_Vm),
    		       last_spike_height otherwise}
  output_data = (Vm < -40 and last_Vm >= -40 and t > 10000)
end

//time is in msec
solver = heun(0.03125)

//setVisible |Vm|
//setVisible |I*|

Vm.setIsVisible(t > 10000)
last_spike_height.setIsVisible(output_data)
last_spike_time.setIsVisible(output_data)
last_spike_area.setIsVisible(output_data)

end
