/*
 Reduced bursting neuron model from Butera et al, 1999.
 Modeled after pre-Botzinger complex (pBC) neuron.
 Copyright 2007 Simatra Modeling Technologies
*/

model (Vm)=pbc(Iext, gNa, gK, gNaP, gleak)
  function xinf(a,b,V) = 1 / (1 + exp((V - a) / b))
  
  // Maximal Conductances (nS)
  input gNa with {default=28}
  input gK with {default=11.2}
  input gNaP with {default=2.8}
  input gleak with {default=2.8}

  // Membrane Parameters
  constant Cmem = .021   //(nF)

  //Time Constants (s)
  constant cTauNah = .010
  constant cTauNaPh = 10
  constant cTauKm = .010

  // Reversal potentials (mV)
  constant ENa = 50   //reported line 35
  constant EK = -85
  constant Eleak = -65

  // Half Activation voltages and slopes (mV)
  constant ThetaNam = -34 
  constant SigmaNam = -5 
  constant ThetaNah = -29 
  constant SigmaNah = 4 
  constant ThetaKm = -29 
  constant SigmaKm = -4 
  constant ThetaNaPm = -40 
  constant SigmaNaPm = -6 
  constant ThetaNaPh = -48 
  constant SigmaNaPh = 6 

  //External Current
  input Iext with {default=0}

  // State Variable Declaration
  state Vm = -60 
  state hNa = 0 
  state hNaP = 0.9 
  state mK = 0.1

  equations
    // Voltage-dependent time   constant and steady state equations
    TauNah = cTauNah/(cosh((Vm-ThetaNah)/(2*SigmaNah)))
    TauNaPh = cTauNaPh/(cosh((Vm-ThetaNaPh)/(2*SigmaNaPh)))
    TauKm = cTauKm/(cosh((Vm-ThetaKm)/(2*SigmaKm)))

    mNa = xinf(ThetaNam, SigmaNam, Vm)
    hNaInf = xinf(ThetaNah, SigmaNah, Vm)
    mNaP = xinf(ThetaNaPm, SigmaNaPm, Vm)
    hNaPInf = xinf(ThetaNaPh, SigmaNaPh, Vm)
    mKInf = xinf(ThetaKm, SigmaKm, Vm)

    //Derived Currents
    INa = gNa*mNa*mNa*mNa*hNa*(Vm - ENa)
    IK = gK*mK*mK*mK*mK*(Vm-EK)
    INaP = gNaP*mNaP*hNaP*(Vm-ENa)
    Ileak = gleak*(Vm-Eleak)

    // Differential Equations
    hNa'= (hNaInf-hNa)/TauNah
    hNaP'= (hNaPInf-hNaP)/TauNaPh
    mK'= (mKInf - mK)/TauKm
    Vm'= 1/Cmem*(-INa-INaP-IK-Ileak+Iext)
  end

  solver = ode23

end

