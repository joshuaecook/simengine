model (x1,x2)=TwoTemporalIteratorTest8

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=rk4{dt=1}}
    state x1 = 0 with {iter=t1}
    state x2 = 1 with {iter=t2}
    
    equation x1' = x2
    equation x2' = x1

end
