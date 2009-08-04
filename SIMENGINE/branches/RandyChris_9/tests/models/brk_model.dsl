// model definition

model BRK
  function cube(x) = x * x * x
  function sqr(x) = x * x

    // Conductances (mS/cm^2)
  constant GNa = 120
  constant GK_dr = 100
  constant GCa_NS = 14
  constant GCa_ND = .03
  constant GK_CaS = 5
  constant GK_CaD = 1.1
  constant GCa_L = 0.33 
  constant gleak = 0.51 

  // Static Parameters
  constant C = 1 
  constant gc = 0.1 // coupling conductance (mS/cm^2)
  constant p = 0.1
  constant Kd = 0.2 // uM
  constant f = 0.01 // percent free to bound Ca
  constant alpha = 0.009 // mol/C/um
  constant kca = 2 // Ca removal rate

  // Half Activation voltages in mV, Slopes in MV, Time Constants in milliseconds
  constant Vhm = -35 
  constant Sm = -7.8 
  constant Vhh = -55 
  constant Sh = 7 
  constant Vhn = -28 
  constant Sn = -15 
  constant VhmN = -30 
  constant SmN = -5 
  constant VhhN = -45 
  constant ShN = 5 
  constant VhmL = -40 
  constant SmL = -7 
  constant TaumN = 4
  constant TauhN = 40
  constant TaumL = 40

  // Reversal potentials in mV
  constant ENa = 55
  constant EK = -80
  constant ECa = 80 
  constant Eleak = -60 
  
  // State Variable Declaration
  state Vs ( -90 to 60 by 0.00001 ) = -60 
  state Vd ( -90 to 60 by 0.00001 ) = -60 
  state h ( 0 to 0.999 by 0.00001 ) = 0.9 
  state n ( 0 to 0.999 by 0.00001 ) = 0 
  state mnS ( 0 to 0.999 by 0.00001 ) = 0 
  state hnS ( 0 to 0.999 by 0.0001 ) = 0.9 
  state mnD ( 0 to 0.999 by 0.000001 ) = 0 
  state hnD ( 0 to 0.999 by 0.0001 ) = 0.9 
  state ml ( 0 to 0.999 by 0.000001 ) = 0 
  state CaS ( 0 to 0.999 by 0.000001 ) = 0 
  state CaD ( 0 to 0.999 by 0.000000001 ) = 0 

  //External Current
  parameter Iext (-40 to 40 by 0.1) = 0
  
  equations
    // Steady state values
    Tauh = 30/(exp((Vs+50)/15)+exp(-(Vs+50)/16))
    Taun = 7/(exp((Vs+40)/40)+exp(-(Vs+40)/50))
    minf = 1/(1+exp((Vs-Vhm)/Sm))
    hinf = 1/(1+exp((Vs-Vhh)/Sh))
    ninf = 1/(1+exp((Vs-Vhn)/Sn))
    mnSinf = 1/(1+exp((Vs-VhmN)/SmN))
    hnSinf = 1/(1+exp((Vs-VhhN)/ShN))
    mnDinf = 1/(1+exp((Vd-VhmN)/SmN))
    hnDinf = 1/(1+exp((Vd-VhhN)/ShN))
    mlinf = 1/(1+exp((Vd-VhmL)/SmL))

    INaS = GNa*minf*sqr(minf)*h*(Vs-ENa)
    IKS = (GK_dr*sqr(sqr(n)) + GK_CaS*CaS/(CaS+Kd))*(Vs-EK)
    ICaS = GCa_NS*mnS*mnS*hnS* (Vs-ECa)//GCa_NS*mnS*mnS*hnS*(Vs-ECa)
    IleakS = gleak*(Vs-Eleak)
    IcouplingS = gc/p*(Vs-Vd)
    IKD = GK_CaD*CaD/(CaD+Kd)*(Vd-EK)
    ICaD = (GCa_ND*mnD*mnD*hnD+GCa_L*ml)*(Vd-ECa)
    IleakD = gleak*(Vd-Eleak)
    IcouplingD = gc/(1-p)*(Vd-Vs)

    // Differential Equations
    h' = (hinf-h)/Tauh
    n' = (ninf-n)/Taun
    mnS' = (mnSinf-mnS)/TaumN
    hnS' = (hnSinf-hnS)/TauhN
    mnD' = (mnDinf-mnD)/TaumN
    hnD' = (hnDinf-hnD)/TauhN
    ml' = (mlinf-ml)/TaumL
    CaS' = f*(-alpha*ICaS-kca*CaS)
    CaD' = f*(-alpha*ICaD-kca*CaD)
    Vs' = 1/C*(Iext-INaS-IKS-ICaS-IleakS-IcouplingS)
    Vd' = 1/C*(-IKD-ICaD-IleakD-IcouplingD)
  end

  solver = forwardeuler(0.002)
  solver.maxduration = 6000

  setVisible |Vs|
  setVisible |Vd|
  setVisible |t|
end
