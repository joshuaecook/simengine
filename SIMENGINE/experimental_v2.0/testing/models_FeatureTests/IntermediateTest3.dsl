model (s,x) = IntermediateTest3(x)

    input x with {default=1}

    state s = 0

    equation s' = 1
    
    solver=forwardeuler{dt=1}

end
