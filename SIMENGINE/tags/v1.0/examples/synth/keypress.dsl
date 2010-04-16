model (key) = keypress()

state key = 0

equation key = {1 when (t > 1000 and t < 4000),
		1 when (t > 5000 and t< 6000),
		1 when (t > 7000 and t < 8000),
		1 when (t > 9000 and t < 9100),
		0 otherwise}

solver = forwardeuler
solver.dt = 1
end
