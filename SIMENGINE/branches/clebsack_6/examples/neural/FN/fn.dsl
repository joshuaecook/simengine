model (u,w) = fn
  constant b0 = 2
  constant b1 = 1.5
  constant e = 0.1
  constant I = 2
  
  state u = 0
  state w = 0

  equations
    u' = u - u*u*u / 3 - w + I
    w' = e * (b0 + b1 * u - w)
  end

  solver = ode45
  solver.dt = 0.1
end
