/*
 Reduced bursting neuron model from Butera et al, 1999.
 Modeled after pre-Botzinger complex (pBC) neuron.
 Copyright 2007 Simatra Modeling Technologies
*/

model pbc
  function xinf(a,b,V) = 1 / (1 + exp((V - a) / b))
  
  // Maximal Conductances (nS)
  parameter gNa (0 to 50 by 0.25) = 28
  parameter gK (0 to 50 by 0.25) = 11.2
  parameter gNaP (0 to 10 by 0.1) = 2.8
  parameter gleak (0 to 10 by 0.1) = 2.8 

  // Membrane Parameters
  parameter Cmem (0 to 100 by 1) = .021   //(nF)

  //Time Constants (s)
  constant cTauNah = .010
  constant cTauNaPh = 10
  constant cTauKm = .010

  // Reversal potentials (mV)
  constant ENa = 50   //reported line 35
  constant EK = -85
  parameter Eleak (-80 to -50 by 0.1) = -65

  // Half Activation voltages and slopes (mV)
  parameter ThetaNam ( -50 to -10 by 0.5) = -34 
  parameter SigmaNam ( -10 to -3 by 0.1 ) = -5 
  parameter ThetaNah ( -50 to -10 by 0.5 ) = -29 
  parameter SigmaNah ( 3 to 10 by 0.1 ) = 4 
  parameter ThetaKm ( -50 to -10 by 0.5 ) = -29 
  parameter SigmaKm ( -20 to -3 by 0.1 ) = -4 
  parameter ThetaNaPm ( -50 to -10 by 0.5 ) = -40 
  parameter SigmaNaPm ( -10 to -3 by 0.1 ) = -6 
  parameter ThetaNaPh ( -50 to -10 by 0.5 ) = -48 
  parameter SigmaNaPh ( 3 to 10 by 0.1 ) = 6 

  //External Current
  parameter Iext (-100 to 100 by 0.1) = 0

  // State Variable Declaration
  visible state Vm ( -90 to 60 by 0.001 ) = -60 
  state hNa ( 0 to 0.999 by 0.0001 ) = 0 
  visible state hNaP ( 0 to 0.999 by 0.0001 ) = 0.9 
  state mK (0 to 0.99 by 0.0001) = 0.1

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


//  t.setPrecision (Range.new(0, 100, 0.00001))

  solver=forwardeuler(0.0001)
//  solver.maxduration = 100
  
  setVisible|I*|
  setInvisible |Iext|
end

