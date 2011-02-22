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
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2) + (c1.Vm - c3.Vm)/((c1.Raxonal + c3.Raxonal)/2)
  c1.Iperiaxonal = (c1.Vmp - c2.Vmp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2) + (c1.Vmp - c3.Vmp)/((c1.Rperiaxonal + c3.Rperiaxonal)/2)
end

model (Vm1, Vm2, Vm3) = axon(Istim)
  constant fiberDiameter = 5.7
  constant paraD1 = 1.9
  constant paraD2 = 3.4
  constant paralength2 = 35
  constant deltax = 500
  constant numLamella = 80
  equation interlength = (deltax - 7 - 2*paralength2)/6
 
  input Istim with {default = 100}

  //instantiate all submodels
  submodel node node1 with {diameter = paraD1}
  submodel mysa mysa1a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut1a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin1a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin1b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin1c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin1d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin1e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin1f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut1b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa1b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  
  submodel node node2 with {diameter = paraD1}
  submodel mysa mysa2a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut2a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin2a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin2b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin2c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin2d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin2e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin2f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut2b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa2b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  
  submodel node node3 with {diameter = paraD1}
  submodel mysa mysa3a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut3a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin3a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin3b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin3c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin3d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin3e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin3f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut3b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa3b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node4 with {diameter = paraD1}
  submodel mysa mysa4a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut4a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin4a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin4b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin4c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin4d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin4e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin4f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut4b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa4b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node5 with {diameter = paraD1}
  submodel mysa mysa5a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut5a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin5a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin5b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin5c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin5d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin5e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin5f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut5b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa5b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node6 with {diameter = paraD1}
  submodel mysa mysa6a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut6a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin6a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin6b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin6c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin6d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin6e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin6f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut6b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa6b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node7 with {diameter = paraD1}
  submodel mysa mysa7a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut7a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin7a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin7b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin7c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin7d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin7e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin7f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut7b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa7b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node8 with {diameter = paraD1}
  submodel mysa mysa8a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut8a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin8a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin8b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin8c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin8d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin8e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin8f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut8b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa8b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node9 with {diameter = paraD1}
  submodel mysa mysa9a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut9a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin9a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin9b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin9c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin9d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin9e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin9f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut9b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa9b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node10 with {diameter = paraD1}
  submodel mysa mysa10a with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut10a with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin10a with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin10b with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin10c with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin10d with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin10e with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel stin stin10f with {diameter = paraD2, length=interlength, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel flut flut10b with {diameter = paraD2, length=paralength2, fiberDiameter = fiberDiameter, numLamella = numLamella}
  submodel mysa mysa10b with {diameter = paraD1, length=3, fiberDiameter = fiberDiameter, numLamella = numLamella}

  submodel node node11 with {diameter = paraD1}

  //stimulus current on first node of axon
  node1.Istim = Istim

  //join all submodels together
  join1(node1, mysa1a)
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
  
  join2(node3, mysa2b, mysa3a)
  join2(mysa3a, node3, flut3a)
  join2(flut3a, mysa3a, stin3a)
  join2(stin3a, flut3a, stin3b)
  join2(stin3b, stin3a, stin3c)
  join2(stin3c, stin3b, stin3d)
  join2(stin3d, stin3c, stin3e)
  join2(stin3e, stin3d, stin3f)
  join2(stin3f, stin3e, flut3b)
  join2(flut3b, stin3f, mysa3b)
  join2(mysa3b, flut3b, node4)

  join2(node4, mysa3b, mysa4a)
  join2(mysa4a, node4, flut4a)
  join2(flut4a, mysa4a, stin4a)
  join2(stin4a, flut4a, stin4b)
  join2(stin4b, stin4a, stin4c)
  join2(stin4c, stin4b, stin4d)
  join2(stin4d, stin4c, stin4e)
  join2(stin4e, stin4d, stin4f)
  join2(stin4f, stin4e, flut4b)
  join2(flut4b, stin4f, mysa4b)
  join2(mysa4b, flut4b, node5)

  join2(node5, mysa4b, mysa5a)
  join2(mysa5a, node5, flut5a)
  join2(flut5a, mysa5a, stin5a)
  join2(stin5a, flut5a, stin5b)
  join2(stin5b, stin5a, stin5c)
  join2(stin5c, stin5b, stin5d)
  join2(stin5d, stin5c, stin5e)
  join2(stin5e, stin5d, stin5f)
  join2(stin5f, stin5e, flut5b)
  join2(flut5b, stin5f, mysa5b)
  join2(mysa5b, flut5b, node6)

  join2(node6, mysa5b, mysa6a)
  join2(mysa6a, node6, flut6a)
  join2(flut6a, mysa6a, stin6a)
  join2(stin6a, flut6a, stin6b)
  join2(stin6b, stin6a, stin6c)
  join2(stin6c, stin6b, stin6d)
  join2(stin6d, stin6c, stin6e)
  join2(stin6e, stin6d, stin6f)
  join2(stin6f, stin6e, flut6b)
  join2(flut6b, stin6f, mysa6b)
  join2(mysa6b, flut6b, node7)

  join2(node7, mysa6b, mysa7a)
  join2(mysa7a, node7, flut7a)
  join2(flut7a, mysa7a, stin7a)
  join2(stin7a, flut7a, stin7b)
  join2(stin7b, stin7a, stin7c)
  join2(stin7c, stin7b, stin7d)
  join2(stin7d, stin7c, stin7e)
  join2(stin7e, stin7d, stin7f)
  join2(stin7f, stin7e, flut7b)
  join2(flut7b, stin7f, mysa7b)
  join2(mysa7b, flut7b, node8)

  join2(node8, mysa7b, mysa8a)
  join2(mysa8a, node8, flut8a)
  join2(flut8a, mysa8a, stin8a)
  join2(stin8a, flut8a, stin8b)
  join2(stin8b, stin8a, stin8c)
  join2(stin8c, stin8b, stin8d)
  join2(stin8d, stin8c, stin8e)
  join2(stin8e, stin8d, stin8f)
  join2(stin8f, stin8e, flut8b)
  join2(flut8b, stin8f, mysa8b)
  join2(mysa8b, flut8b, node9)

  join2(node9, mysa8b, mysa9a)
  join2(mysa9a, node9, flut9a)
  join2(flut9a, mysa9a, stin9a)
  join2(stin9a, flut9a, stin9b)
  join2(stin9b, stin9a, stin9c)
  join2(stin9c, stin9b, stin9d)
  join2(stin9d, stin9c, stin9e)
  join2(stin9e, stin9d, stin9f)
  join2(stin9f, stin9e, flut9b)
  join2(flut9b, stin9f, mysa9b)
  join2(mysa9b, flut9b, node10)

  join2(node10, mysa9b, mysa10a)
  join2(mysa10a, node10, flut10a)
  join2(flut10a, mysa10a, stin10a)
  join2(stin10a, flut10a, stin10b)
  join2(stin10b, stin10a, stin10c)
  join2(stin10c, stin10b, stin10d)
  join2(stin10d, stin10c, stin10e)
  join2(stin10e, stin10d, stin10f)
  join2(stin10f, stin10e, flut10b)
  join2(flut10b, stin10f, mysa10b)
  join2(mysa10b, flut10b, node11)

  join1(node11, mysa10b)

  output Vm1 = node1.Vm
  output Vm2 = node6.Vm
  output Vm3 = node11.Vm

  solver = cvode
  solver.abstol = 1e-3
end
