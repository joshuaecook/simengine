model (u,w) = fn(b0, b1, e, I)

  input b0 with {default=2}
  input b1 with {default=1.5}
  input e with {default=0.1}
  input I with {default=2}
  
  state u = 0
  state w = 0

  equations
    u' = u - u*u*u / 3 - w + I
    w' = e * (b0 + b1 * u - w)
  end

  solver = ode45
//  solver.abstol = 1e-8
  solver.reltol = 1e-5
  solver.dt = 0.1
end
