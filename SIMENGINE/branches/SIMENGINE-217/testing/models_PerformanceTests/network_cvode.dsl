import "gpe.dsl"
import "stn.dsl"
import "gpi.dsl"
import "tc.dsl"

model (spikes) = network_cvode()
  //Definition of submodels
  submodel gpe gpe1
  submodel gpe gpe2
  submodel gpe gpe3
  submodel gpe gpe4
  submodel gpe gpe5

  submodel stn stn1
  submodel stn stn2
  submodel stn stn3
  submodel stn stn4
  submodel stn stn5

  submodel gpi gpi1
  submodel gpi gpi2
  submodel gpi gpi3
  submodel gpi gpi4
  submodel gpi gpi5

  submodel tc tc1
  submodel tc tc2
  submodel tc tc3
  submodel tc tc4
  submodel tc tc5

  //synapses into the GPE group
  gpe1.inhibitoryInput = 0.25*gpe4.spike + 0.5*gpe5.spike + 0.5*gpe2.spike + 0.25*gpe3.spike
  gpe1.excitatoryInput = 0.5*stn5.spike + stn1.spike + 0.5*stn2.spike

  gpe2.inhibitoryInput = 0.25*gpe5.spike + 0.5*gpe1.spike + 0.5*gpe3.spike + 0.25*gpe4.spike
  gpe2.excitatoryInput = 0.5*stn1.spike + stn2.spike + 0.5*stn3.spike

  gpe3.inhibitoryInput = 0.25*gpe1.spike + 0.5*gpe2.spike + 0.5*gpe4.spike + 0.25*gpe5.spike
  gpe3.excitatoryInput = 0.5*stn2.spike + stn3.spike + 0.5*stn4.spike
  
  gpe4.inhibitoryInput = 0.25*gpe2.spike + 0.5*gpe3.spike + 0.5*gpe5.spike + 0.25*gpe1.spike
  gpe4.excitatoryInput = 0.5*stn3.spike + stn4.spike + 0.5*stn5.spike

  gpe4.inhibitoryInput = 0.25*gpe3.spike + 0.5*gpe4.spike + 0.5*gpe1.spike + 0.25*gpe2.spike
  gpe4.excitatoryInput = 0.5*stn4.spike + stn5.spike + 0.5*stn1.spike

  //synapses into the STN group
  stn1.inhibitoryInput = 0.25*stn4.spike + 0.5*stn5.spike + 0.5*stn2.spike + 0.25*stn3.spike +
                         0.25*gpe5.spike + 0.5*gpe1.spike + 0.25*gpe2.spike
  stn2.inhibitoryInput = 0.25*stn5.spike + 0.5*stn1.spike + 0.5*stn3.spike + 0.25*stn4.spike +
                         0.25*gpe1.spike + 0.5*gpe2.spike + 0.25*gpe3.spike
  stn3.inhibitoryInput = 0.25*stn1.spike + 0.5*stn2.spike + 0.5*stn4.spike + 0.25*stn5.spike +
                         0.25*gpe2.spike + 0.5*gpe3.spike + 0.25*gpe4.spike
  stn4.inhibitoryInput = 0.25*stn2.spike + 0.5*stn3.spike + 0.5*stn5.spike + 0.25*stn1.spike +
                         0.25*gpe3.spike + 0.5*gpe4.spike + 0.25*gpe5.spike
  stn5.inhibitoryInput = 0.25*stn3.spike + 0.5*stn4.spike + 0.5*stn1.spike + 0.25*stn2.spike +
                         0.25*gpe4.spike + 0.5*gpe5.spike + 0.25*gpe1.spike

  //synapses into the GPI group
  gpi1.inhibitoryInput = 0.25*gpi4.spike + 0.5*gpi5.spike + 0.5*gpi2.spike + 0.25*gpi3.spike +
                         0.25*gpe5.spike + 0.5*gpi1.spike + 0.25*gpi2.spike
  gpi2.inhibitoryInput = 0.25*gpi5.spike + 0.5*gpi1.spike + 0.5*gpi3.spike + 0.25*gpi4.spike +
                         0.25*gpe1.spike + 0.5*gpi2.spike + 0.25*gpi3.spike
  gpi3.inhibitoryInput = 0.25*gpi1.spike + 0.5*gpi2.spike + 0.5*gpi4.spike + 0.25*gpi5.spike +
                         0.25*gpe2.spike + 0.5*gpi3.spike + 0.25*gpi4.spike
  gpi4.inhibitoryInput = 0.25*gpi2.spike + 0.5*gpi3.spike + 0.5*gpi5.spike + 0.25*gpi1.spike +
                         0.25*gpe3.spike + 0.5*gpi4.spike + 0.25*gpi5.spike
  gpi5.inhibitoryInput = 0.25*gpi3.spike + 0.5*gpi4.spike + 0.5*gpi1.spike + 0.25*gpi2.spike +
                         0.25*gpe4.spike + 0.5*gpi5.spike + 0.25*gpi1.spike
  
  gpi1.excitatoryInput = stn5.spike + stn1.spike + stn2.spike
  gpi2.excitatoryInput = stn1.spike + stn2.spike + stn3.spike
  gpi3.excitatoryInput = stn2.spike + stn3.spike + stn4.spike
  gpi4.excitatoryInput = stn3.spike + stn4.spike + stn5.spike
  gpi5.excitatoryInput = stn4.spike + stn5.spike + stn1.spike

  //synapses onto the TC group
  tc1.inhibitoryInput = gpi1.spike
  tc2.inhibitoryInput = gpi2.spike
  tc3.inhibitoryInput = gpi3.spike
  tc4.inhibitoryInput = gpi4.spike
  tc5.inhibitoryInput = gpi5.spike

  //output Vm = (gpe1.Vm, gpe2.Vm, gpe3.Vm, gpe4.Vm, gpe5.Vm, stn1.Vm, stn2.Vm, stn3.Vm, stn4.Vm, stn5.Vm)
  output spikes = (gpe1.spike, gpe2.spike, gpe3.spike, gpe4.spike, gpe5.spike, stn1.spike, stn2.spike, stn3.spike, stn4.spike, stn5.spike)
  solver = cvode
end
