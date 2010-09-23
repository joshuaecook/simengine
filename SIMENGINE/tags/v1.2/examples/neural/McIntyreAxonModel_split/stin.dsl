/*
Cameron MacIntyre's Axon Model
mysa compartment definition
*/
//************************************************
//special function definitions
//************************************************
model (Vm, Vmp, Raxonal, Rperiaxonal) = stin(fiberDiameter, diameter, length, numLamella, Iaxonal, Iperiaxonal)

//************************************************
//Geometric Parameters
//************************************************
input length with {default = 70.5}
input diameter with {default = 3.4}
input fiberDiameter with {default = 5.7}
input numLamella with {default = 80}
constant paspace = 0.004 					//um
constant pi = 3.1415926

//************************************************
//Electrophysiologic Parameters
//************************************************
constant rhoa = 7e5						    //Ohm*um
equation Ra = rhoa*(1/(diameter/fiberDiameter)^2)/10000  //Ohm*cm
equation cm = 2*diameter/fiberDiameter  //uF/cm^2

equation g_pas = 0.0001*diameter/fiberDiameter //S/cm^2		
constant e_pas = -80                  //mV

equation xraxial = (rhoa*.01)/(pi*((((diameter/2)+paspace)^2)-((diameter/2)^2)))
equation xg = 0.001/(numLamella*2)
equation xc = 0.1/(numLamella*2)

input Iaxonal with {default = 0}
input Iperiaxonal with {default = 0}
//************************************************
//State Declarations
//************************************************
state V (-100 to 50 by 0.00001) = -80 with {iter=t_implicit}
state Vmp (-100 to 50 by 0.00001) = 0 with {iter=t_implicit}

//************************************************
//Model Equations
//************************************************
equations
   //Geometric and membrane parameters of compartment
   SA = (3.1415926*fiberDiameter*length)/1e8                                 //cm^2

   Raxonal = (4*(length/10000)*Ra)/(3.1415926*(fiberDiameter/10000)^2)/1e9   //GOhms
   Rperiaxonal = xraxial*(length/10000)/1e3                             	 //GOhms

   Cm = cm*SA*1e6                                							 //pF
   Cp = xc*SA*1e6                                							 //pF   
   GPas = g_pas*SA*1e9                           							 //nS
   Gmyelin = xg*SA*1e9 									 					 //nS

   //Calculate Passive currents
   IPas = GPas*(V - e_pas)
   Imyelin = Gmyelin*Vmp
   ICmem = -IPas - Iaxonal
   ICmyelin = ICmem + IPas - Imyelin - Iperiaxonal

   //Membrane potential differential equation
   V' = (1/Cm)*(ICmem)
   Vmp' = (1/(Cp))*(ICmyelin)
   Vm = V + Vmp
end

//************************************************
//Simulation Parameters
//************************************************
solver = rk4
solver.dt = 1e-4
end
