model (y) = FunctionTest3

    state x = 0

    equation sqr(x) => x^2

    equation x' = 1
    equation y = sqr(x)    
    
    solver=forwardeuler{dt=1}
end
