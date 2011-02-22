model (y) = FunctionTest2

    state x = 0

    function sqr(x) = x^2

    equation x' = 1
    equation y = sqr(x)    
    
    solver=forwardeuler{dt=1}
end
