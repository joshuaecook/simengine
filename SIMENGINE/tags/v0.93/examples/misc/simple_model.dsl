model (x, y)=simple_model
  output x
  output y

  state x = 0
  state y = 1

  equations
    x' = cos(t)
    y' = -x
  end

  solver = forwardeuler {dt=0.01}
  solver.max_t = 1000
end
