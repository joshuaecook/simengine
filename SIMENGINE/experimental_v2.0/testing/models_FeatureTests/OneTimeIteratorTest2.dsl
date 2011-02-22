model (x)=OneTimeIteratorTest2

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    state x = 0 with {iter=t1}
    
    equation x[t1]' = 1
    
end
