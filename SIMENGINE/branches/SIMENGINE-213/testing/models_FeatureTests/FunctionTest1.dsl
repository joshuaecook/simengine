function sqr(x) = x^2
model (y) = FunctionTest1

    state x = 0

    equation x' = 1
    equation y = sqr(x)    
    
    solver=forwardeuler{dt=1}
end
