model (y) = DerivativeTest2()

      state x = 0
      equation x' = 1
      equation dxdt = x'

      output y = (x, dxdt)

      solver=forwardeuler{dt=1}

end