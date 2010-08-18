// simple model to test the use of time
model (y,z) = time_test()

  state y = 0

  equation y' = {0 when t < 5,
		 1 otherwise}
  equation z = y/2

  output z with {condition=t > 8}

  solver=forwardeuler {dt=0.01}

end
