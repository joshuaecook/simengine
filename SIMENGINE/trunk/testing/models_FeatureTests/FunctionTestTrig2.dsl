model (y) = FunctionTestTrig2

  state x = 0
  equations
      x' = 1
      y1 = asin(x)
      y2 = acos(x)
      y3 = atan(x)
      y4 = acsc(x)
      y5 = asec(x)
      y6 = acot(x)
  end

  output y = (y1, y2, y3, y4, y5, y6)

  solver=forwardeuler{dt=0.1}

end
