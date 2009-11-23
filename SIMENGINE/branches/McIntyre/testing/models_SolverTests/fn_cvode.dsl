//Fitzhugh-Nagumo model of neural excitability

model (u,w) = fn_cvode(b0, b1, e, I)

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

  solver = cvode
  solver.dt = 0.1
end
