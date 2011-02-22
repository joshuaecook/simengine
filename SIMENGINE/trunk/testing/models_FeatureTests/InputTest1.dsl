model (y) = InputTest1(x)
    
    state y = 0
    equation y' = x
    
    solver=forwardeuler{dt=1}
end
