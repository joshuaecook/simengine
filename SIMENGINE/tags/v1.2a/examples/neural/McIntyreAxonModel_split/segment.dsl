import "node.dsl"
import "mysa.dsl"
import "flut.dsl"
import "stin.dsl"

function joinNode(c1, c2)
  //function to join node to another section
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2)
  c1.Iperiaxonal = (c1.Vmp - c2.Vmp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2)
end

function join2(c1, c2, c3)
  //function to join a section to 2 other sections
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2) + (c1.Vm - c3.Vm)/((c1.Raxonal + c3.Raxonal)/2)
  c1.Iperiaxonal = (c1.Vmp - c2.Vmp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2) + (c1.Vmp - c3.Vmp)/((c1.Rperiaxonal + c3.Rperiaxonal)/2)
end

model (Vm1, Vm2, Vm3) = segment(Istim)
  input Istim with {default = 0}
  submodel node node1 with {diameter = 1.9}
  submodel mysa mysa1a with {diameter = 1.9, length=3}
  submodel flut flut1a with {diameter = 3.4, length=35}
  submodel stin stin1a with {diameter = 3.4, length=70}
  submodel stin stin1b with {diameter = 3.4, length=70}
  submodel stin stin1c with {diameter = 3.4, length=70}
  submodel stin stin1d with {diameter = 3.4, length=70}
  submodel stin stin1e with {diameter = 3.4, length=70}
  submodel stin stin1f with {diameter = 3.4, length=70}
  submodel flut flut1b with {diameter = 3.4, length=35}
  submodel mysa mysa1b with {diameter = 1.9, length=3}
  
  submodel node node2 with {diameter = 1.9}
  submodel mysa mysa2a with {diameter = 1.9, length=3}
  submodel flut flut2a with {diameter = 3.4, length=35}
  submodel stin stin2a with {diameter = 3.4, length=70}
  submodel stin stin2b with {diameter = 3.4, length=70}
  submodel stin stin2c with {diameter = 3.4, length=70}
  submodel stin stin2d with {diameter = 3.4, length=70}
  submodel stin stin2e with {diameter = 3.4, length=70}
  submodel stin stin2f with {diameter = 3.4, length=70}
  submodel flut flut2b with {diameter = 3.4, length=35}
  submodel mysa mysa2b with {diameter = 1.9, length=3}
  
  submodel node node3 with {diameter = 1.9}

  node1.Istim = Istim

  joinNode(node1, mysa1a)
  join2(mysa1a, node1, flut1a)
  join2(flut1a, mysa1a, stin1a)
  join2(stin1a, flut1a, stin1b)
  join2(stin1b, stin1a, stin1c)
  join2(stin1c, stin1b, stin1d)
  join2(stin1d, stin1c, stin1e)
  join2(stin1e, stin1d, stin1f)
  join2(stin1f, stin1e, flut1b)
  join2(flut1b, stin1f, mysa1b)
  join2(mysa1b, flut1b, node2)

  join2(node2, mysa1b, mysa2a)
  join2(mysa2a, node2, flut2a)
  join2(flut2a, mysa2a, stin2a)
  join2(stin2a, flut2a, stin2b)
  join2(stin2b, stin2a, stin2c)
  join2(stin2c, stin2b, stin2d)
  join2(stin2d, stin2c, stin2e)
  join2(stin2e, stin2d, stin2f)
  join2(stin2f, stin2e, flut2b)
  join2(flut2b, stin2f, mysa2b)
  join2(mysa2b, flut2b, node3)
  
  joinNode(node3, mysa2b)

  output Vm1 = mysa2a.Vm
  output Vm2 = mysa2a.Vmp
  output Vm3 = stin2a.Vm


  solver = cvode
  solver.dt = 1.6e-7
end
