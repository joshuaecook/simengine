model (s, y) = IntermediateTest2(x)

    input x with {default=1}

    state s = 0

    equation s' = 1
    equation y = x
    
    solver=forwardeuler{dt=1}

end
