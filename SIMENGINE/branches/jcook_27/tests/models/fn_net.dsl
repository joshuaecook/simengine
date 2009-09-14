model fnnode
  parameter b0 (1 to 4 by 0.01) = 2
  parameter b1 (1 to 4 by 0.01) = 1.5
  parameter e (0.01 to 0.3 by 0.01) = 0.1
  parameter I (0 to 5 by 0.01) = 2
  
  state u (-4 to 4 by 0.001) = 1
  state w (-4 to 4 by 0.001) = 1

  state stimulus (-4 to 4 by 0.001) = 0
  
  //function cube (x) = x * x * x

  t.precision = Range.new(0, 1000, 0.01)

  equations
    //d(u) = u - cube(u) / 3 - w + I
    d(u) = u - u * u * u / 3 - w + I + stimulus
    d(w) = e * (b0 + b1 * u - w)
  end

  settings.integrationMethod.dt = dt
  dt.initialval = 0.1
  dt.precision = Range.new(0,1,0.05)
end

model fnnet

  submodel nodes = []

  foreach i in 1 .. 10 do
    nodes.push_back (fnnode.new())
  end 

  foreach n in nodes do
    n.eqs.push_back (equation (n.stimulus = 1))
  end

  settings.integrationMethod.dt = dt
  dt.initialval = 0.1
  dt.precision = Range.new(0,1,0.05)
  t.precision = Range.new(0, 1000, 0.01)

end