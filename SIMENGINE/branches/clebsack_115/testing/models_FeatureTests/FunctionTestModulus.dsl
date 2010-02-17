model (y) = FunctionTestModulus

state x = 0

equation x' = 1
equation y = x % 2

output y

solver=forwardeuler{dt=1}

end

