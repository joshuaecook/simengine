/*
    FitzHugh-Nagumo model of a simplified Hodgkin-Huxley neuron model
    Derived from FitzHugh R. (1955, 1961) and Nagumo J., et al. (1962)
    Copyright 2007-2009 Simatra Modeling Technolgies
*/

model (u,w,I) = fn_test(b0, b1, e)

  input b0 with {default=2}
  input b1 with {default=1.5}
  input e with {default=0.1}
  
  state u = 0
  state w = 0
  state I = 0 with {iter=n}

  state u_roots = 0
  equations
    I[n+1] = I[n] + 0.5
    u' = u - u*u*u / 3 - w + I[n]
    w' = e * (b0 + b1 * u - w)

    u = 5 when u > 5
  end

  solver = ode45
  solver.reltol = 1e-5
  solver.dt = 0.1

//  output fake = u + I
end
