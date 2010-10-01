model (x,y)=PostProcessContinuousIteratorTest4

    state x = 0
    solver = forwardeuler{dt=1}
    
    equation x' = 1
    state y = 0
    equation y = y + 1
    
end

