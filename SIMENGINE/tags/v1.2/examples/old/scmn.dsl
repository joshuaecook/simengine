import "compartment.dsl"

model scmn
  submodel inseg = compartment.new()
  submodel soma = compartment.new()
  submodel d1 = compartment.new()
  submodel d2 = compartment.new()
  submodel d3 = compartment.new()
  submodel d4 = compartment.new()
  
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

  solver = heun(0.0001)
end