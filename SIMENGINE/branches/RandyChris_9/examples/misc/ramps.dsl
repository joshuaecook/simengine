
model (y) = ramps
iterator i = 1..10
state y[i] = i
equation x[i] = i
equation y[i]' = x[i]

solver=forwardeuler {dt=1}

end
