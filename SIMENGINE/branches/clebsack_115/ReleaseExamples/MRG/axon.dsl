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
  segment.Isegmental_R = (segment.VmAxonal_R - node.Vm)/((segment.Raxonal_R + node.Raxonal)/2)
  segment.Iperiaxonal_R = (segment.VmPeriaxonal_R)/((segment.Rperiaxonal_R + node.Rperiaxonal)/2)
  node.Isegmental = -(segment.VmAxonal_R - node.Vm)/((segment.Raxonal_R + node.Raxonal)/2)
end

model (Vm1, Vm2, Vm3) = axon(Istim)
  input Istim with {default = 100}

  // instantiate all submodels
  submodel segment segment1 
  submodel segment segment2
  submodel segment segment3
  submodel segment segment4
  submodel segment segment5
  submodel segment segment6
  submodel segment segment7
  submodel segment segment8
  submodel segment segment9
  submodel segment segment10
  submodel segment segment11 with {Istim = Istim} 
  submodel segment segment12
  submodel segment segment13
  submodel segment segment14
  submodel segment segment15
  submodel segment segment16
  submodel segment segment17
  submodel segment segment18
  submodel segment segment19
  submodel segment segment20
  submodel node node21

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

  output Vm1 = segment1.VmAxonal_L
  output Vm2 = segment11.VmAxonal_L
  output Vm3 = node21.Vm

  solver = cvode
  solver.abstol = 1e-3
end
