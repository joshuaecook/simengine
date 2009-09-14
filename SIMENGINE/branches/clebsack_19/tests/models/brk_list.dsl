import "brk_model.dsl"

model BRKlist

  submodel brks = [BRK.new() foreach x in 1..3]


  solver=heun(0.002)
  solver.maxduration = 6000

  setVisible |*.Vs|
  setVisible |*.Vd|
end
