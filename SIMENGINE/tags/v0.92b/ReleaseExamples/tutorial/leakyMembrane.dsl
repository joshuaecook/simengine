/* 
 *   Tutorial DSL Models
 *   leakyMembrane: RC circuit model of a neuron without active conductances
 *   Copyright 2009 Simatra Modeling Technologies, L.L.C.
 */

model (Vm) = leakyMembrane(Iext)
   input Iext
   
   constant Cm = 0.5
   constant gleak = 8
   constant Eleak = -60

   state Vm = -45

   equations
     Ileak = gleak*(Vm - Eleak)
     Vm' = -(1/Cm)*(Ileak - Iext)
   end

	solver = forwardeuler
   solver.dt = .01
end
