import "segment.dsl"

function join(s1, s2)
  //function to join two segments together s1 is on left and s2 is on right
  //s1.IRight = (s1.VmRight- s2.VmLeft)/((s1.RRight + s2.RLeft)/2)
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

model (Vm1, Vm2, Vm3) = axon_10segment(Istim)
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
  submodel node node11

  join(segment1, segment2)
  join(segment2, segment3)
  join(segment3, segment4)
  join(segment4, segment5)
  join(segment5, segment6)
  join(segment6, segment7)
  join(segment7, segment8)
  join(segment8, segment9)
  join(segment9, segment10)
  joinNode(segment10, node11)

  output Vm1 = segment1.VmAxonal_L
  output Vm2 = segment6.VmAxonal_L
  output Vm3 = node11.Vm

  //solver = cvode
  //solver.abstol = 1e-3
end
