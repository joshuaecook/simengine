/*
 * McIntyre, Richardson, & Grill (J Neurophysiol 87:995-1006, 2001)
 * Publication: Modeling the Excitability of Mammalian Nerve Fibers:
 *              Influence of Afterpotentials on the Recovery Cycle
 *
 * Adapted for use with simEngine
 * Copyright 2009 Simatra Modeling Technologies, L.L.C.
 */

import "segment.dsl"

function join(s1, s2)
  //function to join two segments together s1 is on left and s2 is on right
  s1.Isegmental_R = (s1.VmAxonal_R - s2.VmAxonal_L)/((s1.Raxonal_R + s2.Raxonal_L)/2)
  s1.Iperiaxonal_R = (s1.VmPeriaxonal_R)/((s1.Rperiaxonal_R + s2.Rperiaxonal_L)/2)
  s2.Isegmental_L = (s2.VmAxonal_L - s1.VmAxonal_R)/((s1.Raxonal_R + s2.Raxonal_L)/2)
end

function joinNode(segment, node)
  //function to join a segment to a node at the end of the axon
  segment.Isegmental_R = (segment.VmAxonal_R - node.V)/((segment.Raxonal_R + node.Raxonal)/2)
  segment.Iperiaxonal_R = (segment.VmPeriaxonal_R)/((segment.Rperiaxonal_R + node.Rperiaxonal)/2)
  node.Isegmental = -(segment.VmAxonal_R - node.V)/((segment.Raxonal_R + node.Raxonal)/2)
end

model (spikes) = MRGAxonExt(Istim, Vext1, Vext2, Vext3, Vext4, Vext5, Vext6, Vext7, Vext8, Vext9, Vext10, Vext11, Vext12, Vext13, Vext14, Vext15, Vext16, Vext17, Vext18, Vext19, Vext20, Vext21)

  input Istim with {default = 0}
  input Vext1 with {default = 0}
  input Vext2 with {default = 0}
  input Vext3 with {default = 0}
  input Vext4 with {default = 0}
  input Vext5 with {default = 0}
  input Vext6 with {default = 0}
  input Vext7 with {default = 0}
  input Vext8 with {default = 0}
  input Vext9 with {default = 0}
  input Vext10 with {default = 0}
  input Vext11 with {default = 0}
  input Vext12 with {default = 0}
  input Vext13 with {default = 0}
  input Vext14 with {default = 0}
  input Vext15 with {default = 0}
  input Vext16 with {default = 0}
  input Vext17 with {default = 0}
  input Vext18 with {default = 0}
  input Vext19 with {default = 0}
  input Vext20 with {default = 0}
  input Vext21 with {default = 0}

  iterator t_exp with {continuous, solver=forwardeuler{dt=0.001}}
  iterator t_imp with {continuous, solver=linearbackwardeuler{dt=0.001}}

  // instantiate all submodels
  // deliver local extracellular stimulation to node 11
  submodel segment segment1 with {VextLeft = Vext1, VextRight = Vext2}
  submodel segment segment2 with {VextLeft = Vext2, VextRight = Vext3}
  submodel segment segment3 with {VextLeft = Vext3, VextRight = Vext4}
  submodel segment segment4 with {VextLeft = Vext4, VextRight = Vext5}
  submodel segment segment5 with {VextLeft = Vext5, VextRight = Vext6}
  submodel segment segment6 with {VextLeft = Vext6, VextRight = Vext7}
  submodel segment segment7 with {VextLeft = Vext7, VextRight = Vext8}
  submodel segment segment8 with {VextLeft = Vext8, VextRight = Vext9}
  submodel segment segment9 with {VextLeft = Vext9, VextRight = Vext10}
  submodel segment segment10 with {VextLeft = Vext10, VextRight = Vext11}

  submodel segment segment11 with {VextLeft = Vext11, VextRight = Vext12, Istim = Istim} 

  submodel segment segment12 with {VextLeft = Vext12, VextRight = Vext13}
  submodel segment segment13 with {VextLeft = Vext13, VextRight = Vext14}
  submodel segment segment14 with {VextLeft = Vext14, VextRight = Vext15}
  submodel segment segment15 with {VextLeft = Vext15, VextRight = Vext16}
  submodel segment segment16 with {VextLeft = Vext16, VextRight = Vext17}
  submodel segment segment17 with {VextLeft = Vext17, VextRight = Vext18}
  submodel segment segment18 with {VextLeft = Vext18, VextRight = Vext19}
  submodel segment segment19 with {VextLeft = Vext19, VextRight = Vext20}
  submodel segment segment20 with {VextLeft = Vext20, VextRight = Vext21}

  submodel node node21 with {Vext = Vext21}

  join(segment1, segment2)
  join(segment2, segment3)
  join(segment3, segment4)
  join(segment4, segment5)
  join(segment5, segment6)
  join(segment6, segment7)
  join(segment7, segment8)
  join(segment8, segment9)
  join(segment9, segment10)
  join(segment10, segment11)
  join(segment11, segment12)
  join(segment12, segment13)
  join(segment13, segment14)
  join(segment14, segment15)
  join(segment15, segment16)
  join(segment16, segment17)
  join(segment17, segment18)
  join(segment18, segment19)
  join(segment19, segment20)
  joinNode(segment20, node21)

  equations
    Vm1 = segment1.VmAxonal_L
    spike1 = (Vm1[t_imp[-1]] > Vm1[t_imp[-2]] and
              Vm1[t_imp[-1]] > Vm1[t_imp] and
              Vm1[t_imp[-1]] > -25)

 Vm2 = segment2.VmAxonal_L
    spike2 = (Vm2[t_imp[-1]] > Vm2[t_imp[-2]] and
              Vm2[t_imp[-1]] > Vm2[t_imp] and
              Vm2[t_imp[-1]] > -25)

 Vm3 = segment3.VmAxonal_L
    spike3 = (Vm3[t_imp[-1]] > Vm3[t_imp[-2]] and
              Vm3[t_imp[-1]] > Vm3[t_imp] and
              Vm3[t_imp[-1]] > -25)

    Vm4 = segment4.VmAxonal_L
    spike4 = (Vm4[t_imp[-1]] > Vm4[t_imp[-2]] and
              Vm4[t_imp[-1]] > Vm4[t_imp] and
              Vm4[t_imp[-1]] > -25)

 Vm5 = segment5.VmAxonal_L
    spike5 = (Vm5[t_imp[-1]] > Vm5[t_imp[-2]] and
              Vm5[t_imp[-1]] > Vm5[t_imp] and
              Vm5[t_imp[-1]] > -25)

 Vm6 = segment6.VmAxonal_L
    spike6 = (Vm6[t_imp[-1]] > Vm6[t_imp[-2]] and
              Vm6[t_imp[-1]] > Vm6[t_imp] and
              Vm6[t_imp[-1]] > -25)

 Vm7 = segment7.VmAxonal_L
    spike7 = (Vm7[t_imp[-1]] > Vm7[t_imp[-2]] and
              Vm7[t_imp[-1]] > Vm7[t_imp] and
              Vm7[t_imp[-1]] > -25)

    Vm8 = segment8.VmAxonal_L
    spike8 = (Vm8[t_imp[-1]] > Vm8[t_imp[-2]] and
              Vm8[t_imp[-1]] > Vm8[t_imp] and
              Vm8[t_imp[-1]] > -25)
Vm9 = segment9.VmAxonal_L
    spike9 = (Vm9[t_imp[-1]] > Vm9[t_imp[-2]] and
              Vm9[t_imp[-1]] > Vm9[t_imp] and
              Vm9[t_imp[-1]] > -25)

Vm10 = segment10.VmAxonal_L
    spike10 = (Vm10[t_imp[-1]] > Vm10[t_imp[-2]] and
              Vm10[t_imp[-1]] > Vm10[t_imp] and
              Vm10[t_imp[-1]] > -25)

Vm11 = segment11.VmAxonal_L
    spike11 = (Vm11[t_imp[-1]] > Vm11[t_imp[-2]] and
              Vm11[t_imp[-1]] > Vm11[t_imp] and
              Vm11[t_imp[-1]] > -25)

    Vm12 = segment12.VmAxonal_L
    spike12 = (Vm12[t_imp[-1]] > Vm12[t_imp[-2]] and
              Vm12[t_imp[-1]] > Vm12[t_imp] and
              Vm12[t_imp[-1]] > -25)

Vm13 = segment13.VmAxonal_L
    spike13 = (Vm13[t_imp[-1]] > Vm13[t_imp[-2]] and
              Vm13[t_imp[-1]] > Vm13[t_imp] and
              Vm13[t_imp[-1]] > -25)

Vm14 = segment14.VmAxonal_L
    spike14 = (Vm14[t_imp[-1]] > Vm14[t_imp[-2]] and
              Vm14[t_imp[-1]] > Vm14[t_imp] and
              Vm14[t_imp[-1]] > -25)

Vm15 = segment15.VmAxonal_L
    spike15 = (Vm15[t_imp[-1]] > Vm15[t_imp[-2]] and
              Vm15[t_imp[-1]] > Vm15[t_imp] and
              Vm15[t_imp[-1]] > -25)

    Vm16 = segment16.VmAxonal_L
    spike16 = (Vm16[t_imp[-1]] > Vm16[t_imp[-2]] and
              Vm16[t_imp[-1]] > Vm16[t_imp] and
              Vm16[t_imp[-1]] > -25)
 
Vm17 = segment17.VmAxonal_L
    spike17 = (Vm17[t_imp[-1]] > Vm17[t_imp[-2]] and
              Vm17[t_imp[-1]] > Vm17[t_imp] and
              Vm17[t_imp[-1]] > -25)
  
Vm18 = segment18.VmAxonal_L
    spike18 = (Vm18[t_imp[-1]] > Vm18[t_imp[-2]] and
              Vm18[t_imp[-1]] > Vm18[t_imp] and
              Vm18[t_imp[-1]] > -25)
  
Vm19 = segment19.VmAxonal_L
    spike19 = (Vm19[t_imp[-1]] > Vm19[t_imp[-2]] and
              Vm19[t_imp[-1]] > Vm19[t_imp] and
              Vm19[t_imp[-1]] > -25)
    
Vm20 = segment20.VmAxonal_L
    spike20 = (Vm20[t_imp[-1]] > Vm20[t_imp[-2]] and
              Vm20[t_imp[-1]] > Vm20[t_imp] and
              Vm20[t_imp[-1]] > -25)
  end 
    
  //output Vm = (Vm1)
  output spikes[t_imp] = (spike1, spike2, spike3, spike4, spike5, 
                          spike6, spike7, spike8, spike9, spike10,
                          spike11, spike12, spike13, spike14, spike15,
                          spike16, spike17, spike18, spike19, spike20) when spike1 > 0 or spike2 > 0 or spike3 > 0 or spike4 > 0 or spike5 > 0 or spike6 > 0 or spike7 > 0 or spike8 > 0 or spike9 > 0 or spike10 > 0 or spike11 > 0 or spike12 > 0 or spike13 > 0 or spike14 > 0 or spike15 > 0 or spike16 > 0 or spike17 > 0 or spike18 > 0 or spike19 > 0 or spike20 > 0 

end
