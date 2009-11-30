model (y)=PostProcessContinuousIteratorTest3

    state x = 0
    solver = forwardeuler{dt=1}
    
    equation x' = 1

    output y = (x, x[t[-2]])
    
end

