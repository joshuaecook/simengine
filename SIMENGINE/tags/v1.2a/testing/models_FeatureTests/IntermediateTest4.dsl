model (s,y) = IntermediateTest4

    state s = 0
    equation s' = 1
    equation y = s'
    
    solver=forwardeuler{dt=1}

end
