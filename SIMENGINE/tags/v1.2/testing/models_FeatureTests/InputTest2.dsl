model (y) = InputTest2(x)
    input x with {default=3}
    state y = 0
    equation y' = x
    
    solver=forwardeuler{dt=1}
end
