/*
 * McIntyre, Richardson, & Grill (J Neurophysiol 87:995-1006, 2001)
 * Publication: Modeling the Excitability of Mammalian Nerve Fibers:
 *              Influence of Afterpotentials on the Recovery Cycle
 *
 * Adapted for use with simEngine
 * Copyright 2009 Simatra Modeling Technologies, L.L.C.
 */

import "segment.dsl"

function join1LR(s1, s2)
  //function to join two segments together s1 is left and s2 is right
  s1.IRight = (s1.VmRight- s2.VmLeft)/((s1.RRight + s2.RLeft)/2)
end

function join1RL(s1, s2)
  //function to join two segments together s1 is left and s2 is right
  s1.ILeft = (s1.VmLeft- s2.VmRight)/((s1.RLeft + s2.RRight)/2)
end

function join2(s1, s2, s3)
  //function to join s1 to s2 (left) and s3 (right)
  s1.ILeft = (s1.VmLeft - s2.VmRight)/((s1.RLeft + s2.RRight)/2)
  s1.IRight = (s1.VmRight - s3.VmLeft)/((s1.RRight + s2.RLeft)/2)
end

model (Vm1, Vm2, Vm3) = axon(Istim)
  input Istim with {default = 100}

  //instantiate all submodels
  submodel segment segment1 with {Istim = Istim} 
  submodel segment segment2
  submodel segment segment3
  submodel segment segment4
  submodel segment segment5
  submodel segment segment6
  submodel segment segment7
  submodel segment segment8
  submodel segment segment9
  submodel segment segment10
  submodel segment segment11
  submodel segment segment12
  submodel segment segment13
  submodel segment segment14
  submodel segment segment15
  submodel segment segment16
  submodel segment segment17
  submodel segment segment18
  submodel segment segment19
  submodel segment segment20

  //segment1.node1.length = 1
  //segment20.node2.length = 1

  join1LR(segment1, segment2)
  join2(segment2, segment1, segment3)
  join2(segment3, segment2, segment4)
  join2(segment4, segment3, segment5)
  join2(segment5, segment4, segment6)
  join2(segment6, segment5, segment7)
  join2(segment7, segment6, segment8)
  join2(segment8, segment7, segment9)
  join2(segment9, segment8, segment10)
  join2(segment10, segment9, segment11)
  join2(segment11, segment10, segment12)
  join2(segment12, segment11, segment13)
  join2(segment13, segment12, segment14)
  join2(segment14, segment13, segment15)
  join2(segment15, segment14, segment16)
  join2(segment16, segment15, segment17)
  join2(segment17, segment16, segment18)
  join2(segment18, segment17, segment19)
  join2(segment19, segment18, segment20)
  join1RL(segment20, segment19)

  output Vm1 = segment1.VmLeft
  output Vm2 = segment10.VmRight
  output Vm3 = segment20.VmRight
  
  solver = cvode
  solver.abstol = 1e-3
end
