import "compartment.dsl"

// function join(c1, c2)
//    c1.connect(c2)
//    c2.connect(c1)
// end

function connection(current, previous, next)
  current.I1 = (current.Vm - previous.Vm) / ((current.Raxonal + previous.Raxonal)/2)
  current.I2 = (current.Vm - next.Vm) / ((current.Raxonal + next.Raxonal)/2)
end

function computeCurrent(comp1, comp2)
  I = (comp1.Vm - comp2.Vm)/((comp1.Raxonal + comp2.Raxonal)/2)
end

function two_neighbors(comp, comp1, comp2)
  comp.I1 = computeCurrent(comp, comp1) + computeCurrent(comp, comp2)
end

function three_neighbors(comp, comp1, comp2, comp3)
  comp.I1 = computeCurrent(comp, comp1) + computeCurrent(comp, comp2) + computeCurrent(comp, comp3)
end

model (V) = scmn_big

  submodel compartment inseg
  submodel compartment soma

  submodel compartment d1
  submodel compartment d2
  submodel compartment d3
  submodel compartment d4
  submodel compartment d5
  submodel compartment d6
  submodel compartment d7
  submodel compartment d8
  submodel compartment d9

  submodel compartment d10
  submodel compartment d11
  submodel compartment d12
  submodel compartment d13
  submodel compartment d14
  submodel compartment d15
  submodel compartment d16
  submodel compartment d17
  submodel compartment d18
  submodel compartment d19

  submodel compartment d20
  submodel compartment d21
  submodel compartment d22
  submodel compartment d23
  submodel compartment d24
  submodel compartment d25
  submodel compartment d26
  submodel compartment d27
  submodel compartment d28
  submodel compartment d29

  submodel compartment d30
  submodel compartment d31
  submodel compartment d32
  submodel compartment d33
  submodel compartment d34
  submodel compartment d35
  submodel compartment d36
  submodel compartment d37
  submodel compartment d38
  submodel compartment d39
  submodel compartment d40
  
  inseg.I1 = (inseg.Vm - 0) / ((inseg.Raxonal + 0)/2)
  inseg.I2 = (inseg.Vm - soma.Vm) / ((inseg.Raxonal + soma.Raxonal)/2)

//  connection(inseg, 0, soma)
  connection(soma, inseg, d1)
  connection(d1, soma, d2)
  connection(d2, d1, d3)
  connection(d3, d2, d4)
  connection(d4, d3, d5)
  connection(d5, d4, d6)
  connection(d6, d5, d7)
  connection(d7, d6, d8)
  connection(d8, d7, d9)
  connection(d9, d8, d10)
  connection(d10, d9, d11)

  connection(d11, d10, d12)
  connection(d12, d11, d13)
  connection(d13, d12, d14)
  connection(d14, d13, d15)
  connection(d15, d14, d16)
  connection(d16, d15, d17)
  connection(d17, d16, d18)
  connection(d18, d17, d19)
  connection(d19, d18, d20)
  connection(d20, d19, d21)

  connection(d21, d20, d22)
  connection(d22, d21, d23)
  connection(d23, d22, d24)
  connection(d24, d23, d25)
  connection(d25, d24, d26)
  connection(d26, d25, d27)
  connection(d27, d26, d28)
  connection(d28, d27, d29)
  connection(d29, d28, d30)
  connection(d30, d29, d31)

  connection(d31, d30, d32)
  connection(d32, d31, d33)
  connection(d33, d32, d34)
  connection(d34, d33, d35)
  connection(d35, d34, d36)
  connection(d36, d35, d37)
  connection(d37, d36, d38)
  connection(d38, d37, d39)
  connection(d39, d38, d40)

  d40.I1 = (d40.Vm - d39.Vm) / ((d40.Raxonal + d39.Raxonal)/2)
  d40.I2 = (d40.Vm - 0) / ((d40.Raxonal + 0)/2)

//   inseg.I1 = 0
//   inseg.I2 = (inseg.Vm - soma.Vm) / ((inseg.Raxonal + soma.Raxonal)/2)

//   soma.I1 = (soma.Vm - inseg.Vm) / ((soma.Raxonal + inseg.Raxonal)/2)
//   soma.I2 = (soma.Vm - d1.Vm) / ((soma.Raxonal + d1.Raxonal)/2)

//   d1.I1 = (d1.Vm - soma.Vm) / ((d1.Raxonal + soma.Raxonal)/2)
//   d1.I2 = (d1.Vm - d1.Vm) / ((d1.Raxonal + d1.Raxonal)/2)



//  equation V = soma.Vm
  output V = soma.Vm


/*  submodel d1 = compartment.new()
  submodel d2 = compartment.new()
  submodel d3 = compartment.new()
  submodel d4 = compartment.new()
  submodel d5 = compartment.new()
  submodel d6 = compartment.new()
  submodel d7 = compartment.new()
  submodel d8 = compartment.new()
  submodel d9 = compartment.new()
  submodel d10 = compartment.new()
  submodel d11 = compartment.new()
  submodel d12 = compartment.new()
  submodel d13 = compartment.new()
  submodel d14 = compartment.new()
  submodel d15 = compartment.new()
  submodel d16 = compartment.new()
  submodel d17 = compartment.new()
  submodel d18 = compartment.new()
  submodel d19 = compartment.new()
  submodel d20 = compartment.new()
  submodel d21 = compartment.new()
  submodel d22 = compartment.new()

  submodel dd1 = compartment.new()
  submodel dd2 = compartment.new()
  submodel dd3 = compartment.new()
  submodel dd4 = compartment.new()
   submodel dd5 = compartment.new()
   submodel dd6 = compartment.new()
   submodel dd7 = compartment.new()
   submodel dd8 = compartment.new()
   submodel dd9 = compartment.new()
  submodel dd10 = compartment.new()
  submodel dd11 = compartment.new()
  submodel dd12 = compartment.new()
  submodel dd13 = compartment.new()
  submodel dd14 = compartment.new()
  submodel dd15 = compartment.new()
  submodel dd16 = compartment.new()
  submodel dd17 = compartment.new()
  submodel dd18 = compartment.new()
  submodel dd19 = compartment.new()
  submodel dd20 = compartment.new()
  submodel dd21 = compartment.new()
  submodel dd22 = compartment.new()


  //submodel axon[100]

//  submodel axon[10, 100] = compartment.new()
  

  d[j=_].I = {connect(0, d[j+1].I)        when j==1, 
              connect(d[j-1].I, 0)        when j==x.last,
              connect(d[j-1].I, d[j+1].I) otherwise})

  for 

  inseg.connect(soma)
  soma.connect(inseg)
  
  soma.connect(d1)

  d1.connect(soma)

  d1.connect(d2)
  d2.connect(d1)

  d2.connect(d3)
  d3.connect(d2)

  d3.connect(d4)
  d4.connect(d3)

  d4.connect(d5)
  d5.connect(d4)
  
  d5.connect(d6)
  d6.connect(d5)

  d6.connect(d7)
  d7.connect(d6)
  
  d7.connect(d8)
  d8.connect(d7)
  
  d8.connect(d9)
  d9.connect(d8)
  
  d9.connect(d10)
  d10.connect(d9)
  
  d10.connect(d11)
  d11.connect(d10)
  
  d11.connect(d12)
  d12.connect(d11)
  
  d12.connect(d13)
  d13.connect(d12)
  
  d13.connect(d14)
  d14.connect(d13)
  
  d14.connect(d15)
  d15.connect(d14)
  
  d15.connect(d16)
  d16.connect(d15)
  
  d16.connect(d17)
  d17.connect(d16)
  
  d17.connect(d18)
  d18.connect(d17)
  
  d18.connect(d19)
  d19.connect(d18)
  
  d19.connect(d20)
  d20.connect(d19)
  
  d20.connect(d21)
  d21.connect(d20)
  
  d21.connect(d22)
  d22.connect(d21)

  d22.connect(dd1)
  dd1.connect(d22)


  dd1.connect(dd2)
  dd2.connect(dd1)

  dd2.connect(dd3)
  dd3.connect(dd2)

  dd3.connect(dd4)
  dd4.connect(dd3)

  dd4.connect(dd5)
  dd5.connect(dd4)
  
  dd5.connect(dd6)
  dd6.connect(dd5)

  dd6.connect(dd7)
  dd7.connect(dd6)
  
  dd7.connect(dd8)
  dd8.connect(dd7)
  
  dd8.connect(dd9)
  dd9.connect(dd8)
  
  dd9.connect(dd10)
  dd10.connect(dd9)
  
  dd10.connect(dd11)
  dd11.connect(dd10)
  
  dd11.connect(dd12)
  dd12.connect(dd11)
  
  dd12.connect(dd13)
  dd13.connect(dd12)
  
  dd13.connect(dd14)
  dd14.connect(dd13)
  
  dd14.connect(dd15)
  dd15.connect(dd14)
  
  dd15.connect(dd16)
  dd16.connect(dd15)
  
  dd16.connect(dd17)
  dd17.connect(dd16)
  
  dd17.connect(dd18)
  dd18.connect(dd17)
  
  dd18.connect(dd19)
  dd19.connect(dd18)
  
  dd19.connect(dd20)
  dd20.connect(dd19)
  
  dd20.connect(dd21)
  dd21.connect(dd20)
  
  dd21.connect(dd22)
  dd22.connect(dd21)
*/  

  solver = ode23//(0.0001, 1e-3, 1e-6)


end

//compile(scmn_big)
/*

model Vms = scmn


  maximum_x = 4
  iterator x with {range=1..maximum_x}
  (denVm[x], denI[x]) = {compartment(somaI, denI[x+1])    with {name=firstd}  when x == 1,
                         compartment(denI[x-1], 0)        with {name=lastd}   when x == maximum_x,
                         compartment(denI[x-1],denI[x+1]) with {name=d[x]}    otherwise}




  (insegVm, insegI) = compartment(0, somaI)
  (somaVm, somaI) = compartment(insegI, denI[1])

  maximum_x = 4
  iterator x with {range=1..maximum_x}
  (denVm[x], denI[x]) = {compartment(somaI, denI[x+1])    when x == 1,
                         compartment(denI[x-1], 0)        when x == maximum_x,
                         compartment(denI[x-1],denI[_])   otherwise}





  maximum_x = 4
  mycomp[maximum_x] = compartment.new()

  submodel compartment[maximum_x] mycomp

  (denVm[x], denI[x]) = 

  iterator x with {range=1..maximum_x}
  (denVm[x], denI[x]) = {compartment(somaI, denI[x+1])    when x == 1,
                         compartment(denI[x-1], 0)        when x == maximum_x,
                         compartment(denI[x-1],denI[_])   otherwise} + (myVm, myI)

  {denVm[x], denI[x]) = switch(x, 
                               1, compartment ...
                               maximum_x, comp...
                               else, comp....)

  I[1] = compartment(somaI, I[2])
  I[x.max] = compartment(I[x.max-1], 0)
  I[j=2..x.max-1] = compartment(I[j-1], I[j+1])// + otherSubModel(3)


  submodel soma (Hello, World) = compartment(insegI, denI[1])
  soma.Hello, soma.World

  quantity denVm[1..10], denI with {iterator=[t,x]}

  Vm[i=1..10] = 

  (denVm[t,1], denI[t,1]) = compartment(somaI,   denI[2]) with {name=d1}
  (denVm[t,2], denI[t,2]) = compartment(denI[1], denI[3]) with {name=d2}
  (denVm[t,3], denI[t,3]) = compartment(denI[2], denI[4]) with {name=d3}
  (denVm[t,4], denI[t,4]) = compartment(denI[3], 0)       with {name=d4}

  denVm[t,4] == denVm[4]
  denVm[0,_] == init(denVm)

end*/