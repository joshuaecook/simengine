/*  Lobster STG pacing neuron model
    Derived from Prinz et al, J Neurophysiol, December 2003
    Copyright 2008 Simatra Modeling Technologies
*/
function xinf(a, b, V) = 1/(1 + exp((V + a)/b))
function taux(a, b, c, e, V) = c + e / (1 + exp((V + a)/b))
function cube (x) = x * x * x
function sqr (x) = x * x

//*****************************************STG***********************************************//
model (Vm) = stg

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

constant ENa = 50 //mV
constant EK = -80
constant Eh = -20
constant Eleak = -50
constant ECa = 125

parameter Vclamp (0 to 1 by 1) = 0
parameter Vclamp_cmd (-100 to 100 by 0.1) = 0

//State Variable Declaration
state Vm (-80 to 50 by 0.0001) = -50 //mV
state mNa (0 to 0.999 by 0.0001 ) = 0
state hNa (0 to 0.999 by 0.0001 ) = 0
state mCaT (0 to 0.999 by 0.001 ) = 0
state hCaT (0 to 0.999 by 0.00001 ) = 0
state mCaS (0 to 0.999 by 0.001 ) = 0
state hCaS (0 to 0.999 by 0.000001 ) = 0
state mA (0 to 0.999 by 0.0001 ) = 0
state hA (0 to 0.999 by 0.00001 ) = 0
state mKCa (0 to 0.999 by 0.00001 ) = 0
state mKd (0 to 0.999 by 0.0001 ) = 0
state mh (0 to 0.999 by 0.0000001 ) = 0
state Caconc (0.05 to 300 by 0.001) = 100 //uM

equations
   //Nernst Equation to calculate Calcium Reversal
   //ECa = 29.55*log(3000/Caconc)
   
   //Ionic Currents in uA
   INa = gNa*cube(mNa)*hNa*(Vm - ENa)*Amem
   ICaT = gCaT*cube(mCaT)*hCaT*(Vm - ECa)*Amem
   ICaS = gCaS*cube(mCaS)*hCaS*(Vm - ECa)*Amem
   IA = gA*cube(mA)*hA*(Vm-EK)*Amem
   IKCa = gKCa*sqr(mKCa)*sqr(mKCa)*(Vm-EK)*Amem
   IKd = gKd*sqr(mKd)*sqr(mKd)*(Vm-EK)*Amem
   Ih = gh*(Vm - Eh)*Amem
   Ileak = gleak*(Vm-Eleak)*Amem

   // Differential Equations
   // Time constants are in msec
   Caconc' = (1/200)*(-14960*(ICaT + ICaS) - Caconc + 0.05) //uM for CaConc
   mNa' = (xinf(25.5, -5.29, Vm) - mNa)/(taux(120, -25, 2.64, -2.52, Vm))
   hNa' = (xinf(48.9, 5.18, Vm) - hNa)/(taux(62.9, -10, 0, 1.34, Vm)*taux(34.9, 3.6, 1.5, 1, Vm))
   //temp1 = (xinf(48.9, 5.18, Vm) - hNa)
   //temp2 = 1/(taux(62.9, -10, 0, 1.34, Vm)*taux(34.9, 3.6, 1.5, 1, Vm))
   //hNa' = temp1 * temp2
   mCaT' = (xinf(27.1, -7.2, Vm) - mCaT)/(taux(68.1, -20.5, 43.4, -42.6, Vm))
   hCaT' = (xinf(32.1, 5.5, Vm) - hCaT)/(taux(55, -16.9, 210, -179.6, Vm))
   mCaS' = (xinf(33, -8.1, Vm) - mCaS)/(2.8 + 14/(exp((Vm +27)/10) + exp((Vm + 70)/-13)))
   hCaS' = (xinf(60, 6.2, Vm) - hCaS)/(120 + 300/(exp((Vm + 55)/9) + exp((Vm+65)/-16)))
   mA' = (xinf(27.2, -8.7, Vm) - mA)/(taux(32.9, -15.2, 23.2, -20.8, Vm))
   hA' = (xinf(56.9, 4.9, Vm) - hA)/(taux(38.9, -26.5, 77.2, -58.4, Vm))
   mKCa' = (((Caconc/1)/((Caconc/1) + 3))*xinf(28.3, -12.6, Vm) - mKCa)/(taux(46, -22.7, 180.6, -150.2, Vm))
   mKd' = (xinf(12.3, -11.8, Vm) - mKd)/(taux(28.3, -19.2, 14.4, -12.8, Vm))
   mh' = (xinf(75, 5.5, Vm) - mh)/(2/(exp((Vm + 169.7)/-11.6) + exp((Vm - 26.7)/14.3)))
   Vm' = (1/Cmem*(-INa-ICaT-ICaS-IA-IKCa-IKd-Ih-Ileak))
   //Vm' = (1/Cmem*(-INa-IKd-Ih))
   //Vm' = (1/Cmem*(-INa-Ih))
end

//t.setPrecision (Range.new(0, 100000, 0.005))
solver = rk4(0.01)
//solver.maxduration = 100000

/*setVisible |Vm|
//setVisible |INa|
//setVisible |mNa|
//setVisible |hNa|
//setVisible |temp?|
setVisible |m*|
setVisible |h*|
*/
end

