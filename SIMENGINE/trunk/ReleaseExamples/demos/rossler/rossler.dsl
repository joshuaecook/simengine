model (x,y,z) = rossler(a,b,c)
  input a with {default=0.2}
  input b with {default=0.2}
  input c with {default=5.7}

  state x = 0
  state y = 0
  state z = 0

  equations
    x' = -y - z
    y' = x + a * y
    z' = b + z * (x - c)
  end

  solver = ode45 {reltol=1e-8, abstol=1e-8}

end
