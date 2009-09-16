model (y) = FunctionTestTrig4

  state x = 0
  equations
      x' = 1
      y1 = asinh(x)
      y2 = acosh(x)
      y3 = atanh(x)
      y4 = acsch(x)
      y5 = asech(x)
      y6 = acoth(x)
  end

  output y = (y1, y2, y3, y4, y5, y6)

  solver=forwardeuler{dt=0.1}

end
