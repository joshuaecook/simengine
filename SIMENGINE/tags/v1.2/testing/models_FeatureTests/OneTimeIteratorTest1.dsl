model (x)=OneTimeIteratorTest1

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    state x = 0 with {iter=t1}
    
    equation x' = 1
    
end
