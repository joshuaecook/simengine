/*
 * Cameron MacIntyre's Axon Model
 * mysa compartment definition
 * adapted for use with simEngine
 *
 * Copyright 2009 Simatra Modeling Technologies, L.L.C.
 */

//************************************************
//special function definitions
//************************************************
model (V, Vp, Raxonal, Rperiaxonal) = mysa(fiberDiameter, diameter, length, numLamella, Iaxonal, Iperiaxonal, Isegmental_axonal, Isegmental_periaxonal, Vext)

iterator t_exp with {continuous, solver=forwardeuler{dt=0.001}}
iterator t_imp with {continuous, solver=linearbackwardeuler{dt=0.001}}

//************************************************
//Geometric Parameters
//************************************************
input length with {default = 3}
input diameter with {default = 1.9}
input fiberDiameter with {default = 5.7}
input numLamella with {default = 80}
input Vext with {default = 0}

constant paspace = 0.002 					//um
constant pi = 3.1415926

//************************************************
//Electrophysiologic Parameters
//************************************************
constant rhoa = 7e5						    //Ohm*um
equation Ra = rhoa*(1/(diameter/fiberDiameter)^2)/10000  //Ohm*cm
equation cm = 2*diameter/fiberDiameter  //uF/cm^2

equation g_pas = 0.001*diameter/fiberDiameter //S/cm^2		
constant e_pas = -80                  //mV

equation xraxial = (rhoa*.01)/(pi*((((diameter/2)+paspace)^2)-((diameter/2)^2)))
equation xg = 0.001/(numLamella*2)
equation xc = 0.1/(numLamella*2)

input Iaxonal with {default = 0}
input Iperiaxonal with {default = 0}
input Isegmental_axonal with {default = 0}
input Isegmental_periaxonal with {default = 0}

//************************************************
//State Declarations
//************************************************
state Vmem = -80 with {iter=t_imp}
state Vmp = 0 with {iter=t_imp}

//************************************************
//Model Equations
//************************************************
equations
   //Geometric and membrane parameters of compartment
   SA = (3.1415926*fiberDiameter*length)/1e8                                 //cm^2
   
   Raxonal = (4*(length/10000)*Ra)/(3.1415926*(fiberDiameter/10000)^2)/1e9   //GOhms
   Rperiaxonal = xraxial*(length/10000)/1e3                             //GOhms

   Cm = cm*SA*1e6                                //pF
   Cp = xc*SA*1e6                                //pF   
   GPas = g_pas*SA*1e9                           //nS
   Gmyelin = xg*SA*1e9 									 //nS

   //Calculate Passive currents
   IPas = GPas*(Vmem - e_pas)
   Imyelin = Gmyelin*Vmp
   ICmem = -IPas - Iaxonal - Isegmental_axonal
   ICmyelin = ICmem + IPas - Imyelin - Iperiaxonal - Isegmental_periaxonal

   //Membrane potential differential equation
   Vmem' = (1/Cm)*(ICmem)
   Vmp' = (1/(Cp))*(ICmyelin)
   Vm = Vmem + Vmp
end

output V = Vm + Vext
output Vp = Vmp + Vext
end
