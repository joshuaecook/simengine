import "compartment.dsl"

// function join(c1, c2)
//    c1.connect(c2)
//    c2.connect(c1)
// end

model (V) = scmn2(Iapp)

  input Iapp with {default=20}

  submodel compartment inseg
  submodel compartment soma
  
  inseg.I1 = 0
  inseg.I2 = (inseg.Vm - soma.Vm) / ((inseg.Raxonal + soma.Raxonal)/2)

  soma.I1 = (soma.Vm - inseg.Vm) / ((soma.Raxonal + inseg.Raxonal)/2)
  soma.I2 = -Iapp

  equation V = soma.Vm


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

//  solver = ode23(0.0001, 1e-3, 1e-6)
  solver = ode45//(0.0001, 1e-3, 1e-6)
  solver.abstol = 1e-4
  solver.reltol = 1e-7
//  solver = rk4(0.0001)


end

compile(scmn2)
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