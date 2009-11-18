import "node.dsl"
import "mysa.dsl"
import "flut.dsl"
import "stin.dsl"

function join1(c1, c2)
  //function to join node to another section
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2)
  c1.Iperiaxonal = (c1.Vmp - c2.Vmp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2)
end

function join2(c1, c2, c3)
  //function to join a section to 2 other sections
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2) + (c1.Vm - c3.Vm)/((c1.Raxonal +c3.Raxonal)/2)
  c1.Iperiaxonal = (c1.Vmp - c2.Vmp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2) +     (c1.Vmp-c3.Vmp)/((c1.Rperiaxonal + c3.Rperiaxonal)/2)
end

model (VmLeft, VmRight, RLeft, RRight) = segment(Istim, ILeft, IRight)
  input Istim with {default = 0}
  input ILeft with {default = 0}
  input IRight with {default = 0}

  submodel node node1 with {diameter = 1.9, length = 0.5, Isegmental = ILeft}
  submodel mysa mysa1 with {diameter = 1.9, length=3}
  submodel flut flut1 with {diameter = 3.4, length=35}
  submodel stin stin1 with {diameter = 3.4, length=70}
  submodel stin stin2 with {diameter = 3.4, length=70}
  submodel stin stin3 with {diameter = 3.4, length=70}
  submodel stin stin4 with {diameter = 3.4, length=70}
  submodel stin stin5 with {diameter = 3.4, length=70}
  submodel stin stin6 with {diameter = 3.4, length=70}
  submodel flut flut2 with {diameter = 3.4, length=35}
  submodel mysa mysa2 with {diameter = 1.9, length=3}
  submodel node node2 with {diameter = 1.9, length = 0.5, Isegmental = IRight}

  node1.Istim = Istim

  join1(node1, mysa1)
  join2(mysa1, node1, flut1)
  join2(flut1, mysa1, stin1)
  join2(stin1, flut1, stin2)
  join2(stin2, stin1, stin3)
  join2(stin3, stin2, stin4)
  join2(stin4, stin3, stin5)
  join2(stin5, stin4, stin6)
  join2(stin6, stin5, flut2)
  join2(flut2, stin6, mysa2)
  join2(mysa2, flut2, node2)
  join1(node2, mysa2)

  output VmLeft = node1.Vm
  output VmRight = node2.Vm
  output RLeft = node1.Raxonal
  output RRight = node2.Raxonal

  solver = cvode
  solver.reltol = 1e-3
end
