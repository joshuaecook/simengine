model (y) = FunctionTestTrig3

  state x = 0
  equations
      x' = 1
      y1 = sinh(x)
      y2 = cosh(x)
      y3 = tanh(x)
      y4 = csch(x)
      y5 = sech(x)
      y6 = coth(x)
  end

  output y = (y1, y2, y3, y4, y5, y6)

  solver=forwardeuler{dt=0.1}

end
