model (y) = DerivativeTest4()

      state x = 0
      equation x' = 1

      output y = (x, x')

      solver=linearbackwardeuler{dt=1}

end