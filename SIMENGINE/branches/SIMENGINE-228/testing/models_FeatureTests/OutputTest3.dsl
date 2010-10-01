model (y) = OutputTest3

state x = 0

equation x' = 1
equation neg_x = -x

output y = (x, neg_x)

solver=forwardeuler{dt=1}

end

