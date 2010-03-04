model (y) = DerivativeTest1()

      state x = 0
      equation x' = 1

      output y = (x, x')

      solver=forwardeuler{dt=1}

end