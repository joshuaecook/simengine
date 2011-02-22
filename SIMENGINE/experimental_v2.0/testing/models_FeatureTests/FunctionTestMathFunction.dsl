model (x,y,z) = FunctionTestMathFunction

  state x = 0
  equations
    x' = 0.01
    f(x) => x + 1

    y = f(x)
    z = f(y)
  end

  solver = forwardeuler{dt=1}

end