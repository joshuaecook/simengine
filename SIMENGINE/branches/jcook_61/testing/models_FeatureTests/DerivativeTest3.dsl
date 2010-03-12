model (y) = DerivativeTest3()

      state x = 0
      equation dxdt = 1
      equation x' = dxdt

      output y = (x, dxdt)

      solver=forwardeuler{dt=1}

end