/* 
 *   Tutorial DSL Models
 *   neuron: Hodgkin-Huxley variety neuron model
 *   Copyright 2009 Simatra Modeling Technologies, L.L.C.
 */

model (Vm) = neuron(Iext)
   input Iext with {default = 0}

   constant Cm = 1

   constant gNa = 120
   constant gK = 100
   constant gleak = .51

   constant ENa = 55
   constant EK = -80
   constant Eleak = -55

   state Vm = -45
   state hNa = 0.9
   state mK = 0.1

   equations
     mNaInf = 1/(1+exp((Vm+35)/-7.8))
     hNaInf = 1/(1+exp((Vm+55)/7))
     mKInf = 1/(1+exp((Vm+28)/-15))
     
     hNaTau = 30/(exp((Vm+50)/15)+exp(-(Vm+50)/16))
     mKTau = 7/(exp((Vm+40)/40)+exp(-(Vm+40)/50))

     INa = gNa*mNaInf^3*hNa*(Vm - ENa)
     IK = gK*mK^4*(Vm - EK)
     Ileak = gleak*(Vm - Eleak)

     hNa' = (hNaInf - hNa)/hNaTau
     mK' = (mKInf - mK)/mKTau
     Vm' = -(1/Cm)*(INa + IK + Ileak -Iext)
   end

   solver = forwardeuler
   solver.dt = .001
end
