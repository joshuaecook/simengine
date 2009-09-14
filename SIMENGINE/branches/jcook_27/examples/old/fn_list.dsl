import "fn.dsl"

model FNlist

  submodel fns = [fn.new() foreach x in 1..3]


  solver=heun(0.1)
  solver.maxduration = 6000

  setVisible |*.u|
  setVisible |*.w|
end
