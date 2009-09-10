// Lorenz Attractor (see Lorenz, E.N., 1963)
// Copyright 2009 Simatra Modeling Technologies

model (x, y, z) = 
      lorenz(sigma, rho, beta)

  input sigma with {default=10}
  input rho with {default=28}
  input beta with {default=3}

  state x = 0.001
  state y = 1
  state z = 1.05

  equations
    x' = sigma*(y-x)
    y' = x*(rho-z)-y
    z' = x*y - beta*z
  end

  solver=cvode

end
