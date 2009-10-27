model (x)=UpdateContinuousIteratorTest2

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    state x = 0 with {iter=t1}
    
    equation x' = 1
    equation x = 0 when t1 >= 4
    
end
