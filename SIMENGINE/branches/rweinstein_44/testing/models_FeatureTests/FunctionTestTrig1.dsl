model (y) = FunctionTestTrig1

  state x = 0
  equations
      x' = 1
      y1 = sin(x)
      y2 = cos(x)
      y3 = tan(x)
      y4 = csc(x)
      y5 = sec(x)
      y6 = cot(x)
  end

  output y = (y1, y2, y3, y4, y5, y6)

  solver=forwardeuler{dt=0.1}

end
