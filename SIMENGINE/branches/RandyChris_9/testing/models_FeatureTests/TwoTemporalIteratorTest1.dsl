model (y)=TwoTemporalIteratorTest1

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=forwardeuler{dt=1}}
    state x1 = 0 with {iter=t1}
    state x2 = 0 with {iter=t2}
    
    equation x1' = 1
    equation x2' = 2

    output y = (x1, x2)

end
