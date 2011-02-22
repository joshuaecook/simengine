model (Vm, spikesOut) = ML(spikesIn, ELeak, I)

input ELeak with {default = -53.23878}
input I with {default = 0}
input spikesIn with {default = 0}

state Vm =-60
state n = 0
state Ser = 0
state Sef = 0

constant C = 1
constant gLeak = 8
constant gNa = 18.22315
constant ENa = 60
constant gK = 4
constant EK = -95.52116

constant Vhalfm = -7.37257
constant km = 11.97239
constant Vhalfn = -16.34877
constant kn = 4.21133
constant tau = 1

constant E_EPSP = 0
constant tauEPSPr = 0.25
constant tauEPSPf = 0.5

constant dt = .02
random noise with {normal, mean=0, stddev=1}
//constant noise = 1

equations
  gEPSP = 0.02/(tauEPSPf - tauEPSPr)

  noiseScalar = .2/sqrt(dt)

  Vm' = (1/C)*(I-gLeak*(Vm-ELeak) 
             - gNa*(1.0/(1.0 + exp((Vhalfm - Vm)/km )))*(Vm-ENa) 
             - gK*n*(Vm-EK) + gEPSP*(Sef-Ser)*(E_EPSP-Vm)) + noise*noiseScalar
  
n' =   (1.0/(1.0+exp((Vhalfn - Vm)/kn))-n)/exp(-0.07*Vm-3)

  Ser' = -Ser/tauEPSPr
  Ser = Ser + spikesIn when spikesIn > 0

  Sef' = -Sef/tauEPSPf
  Sef = Sef + spikesIn when spikesIn > 0

  spike = (Vm > -20 and Vm[t[-1]] <= -20)
end

output spikesOut = spike with {condition = spike > 0}
solver = forwardeuler
solver.dt = 0.02
end
