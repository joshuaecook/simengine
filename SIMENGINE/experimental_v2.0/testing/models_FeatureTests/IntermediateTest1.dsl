model (x, y) = IntermediateTest1

    state x = 0

    equation x' = 1
    equation y = x
    
    solver=forwardeuler{dt=1}
end
