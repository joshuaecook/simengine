/*
 * Cameron MacIntyre's Axon Model
 * node compartment definition
 * adapted for use with simEngine
 * 
 * Copyright 2009 Simatra Modeling Technologies, L.L.C.
 */

model (V, Vp, Raxonal, Rperiaxonal) = node(diameter, length, Istim, Iaxonal, Iperiaxonal, Isegmental, Vext)

iterator t_exp with {continuous, solver=forwardeuler{dt=0.001}}
iterator t_imp with {continuous, solver=linearbackwardeuler{dt=0.001}}

//time is in msec
//************************************************
//Geometric Parameters
//************************************************
input length with {default = 1}
input diameter with {default = 1.9}
input Isegmental with {default = 0}
input Vext with {default = 0}

constant paspace = 0.002 					//um
constant pi = 3.1415926

//************************************************
//Electrophysiologic Parameters
//************************************************
constant rhoa = 7e5						    //Ohm*um
equation Ra = rhoa/10000  					 //Ohm*cm
constant cm = 2 								 //uF/cm^2

constant g_pas = 0.007
constant e_pas = -90                  //mV

equation xraxial = (rhoa*.01)/(pi*((((diameter/2)+paspace)^2)-((diameter/2)^2)))

constant gNaF = 3.0 //S/cm^2
constant gKs = 0.08 //S/cm^2
constant gNaP = 0.01 //S/cm^2

constant ENa = 50 //mV
constant EK = -90 //mV

input Istim with {default = 0} //pA
input Iaxonal with {default = 0}
input Iperiaxonal with {default = 0}
//************************************************
//State Declarations
//************************************************
state Vm = -80 with {iter=t_imp}
state m  = 0.0732093 with {iter=t_exp} //0.0679
state h  = 0.620695 with {iter=t_exp} //0.6209
state p  = 0.202604 with {iter=t_exp}
state s = 0.0430299 with {iter=t_exp}

constant Vmp = 0

//************************************************
//Model Equations
//************************************************

equations
   //Geometric and membrane parameters of compartment
   SA = (3.1415926*diameter*length)/1e8                                 //cm^2
   
   Raxonal = (4*(length/10000)*Ra)/(3.1415926*(diameter/10000)^2)/1e9   //GOhms
   Rperiaxonal = xraxial*(length/10000)/1e3                             //GOhms

   Cm = cm*SA*1e6                                //pF
   GPas = g_pas*SA*1e9                           //nS

   GNaF = gNaF*SA*1e9 									  //nS
   GKa = gKs*SA*1e9 									  //nS
   GNaP = gNaP*SA*1e9 									  //nS
   
   //Equations for fast sodium current (pA)
   alpha_m = (6.57 * (Vm + 21.4))/(1 - exp(-(Vm + 21.4)/10.3))
   beta_m = (0.304 * (-(Vm +25.7)))/(1 - exp((Vm + 25.7)/9.16))
   alpha_h = (0.34 * (-(Vm + 114)))/(1 - exp((Vm + 114)/11))
   beta_h = 12.6/(1 + exp(-(Vm + 31.8)/13.4))
   m' = alpha_m*(1 - m) - beta_m*m
   h' = alpha_h*(1 - h) - beta_h*h
   INaF = GNaF*m*m*m*h*(Vm - ENa) //pA

   //Equations for persistant sodium current (pA)
   alpha_p = (0.0353 * (Vm + 27))/(1 - exp(-1*(Vm + 27)/10.2))
   beta_p = (0.000883 * (-(Vm + 34)))/(1 - exp((Vm + 34)/10))
   p' = alpha_p*(1-p) - beta_p*p
   INaP = GNaP*p*p*p*(Vm - ENa) //pA

   //Equations for slow potassium current (pA)
   alpha_s = 0.3/(1 + exp(-(Vm + 53)/5))
   beta_s = 0.03/(1 + exp(-(Vm + 90)))
   s' = alpha_s*(1 - s) - beta_s*s
   IK = GKa*s*(Vm - EK) //pA

   //Leak current (pA)
   IPas = GPas*(Vm - e_pas)

   //summation of membrane currents
   ICmem = Istim - IPas - Iaxonal - Isegmental - INaF - INaP -IK

   //Membrane potential differential equation   
   Vm' = (1/Cm)*(ICmem)
end

output V = Vm + Vext
output Vp = Vmp + Vext
end
