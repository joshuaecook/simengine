model (y)=TwoTemporalIteratorTest7

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=rk4{dt=1}}
    state x1 = 0 with {iter=t1}
    state x2 = 0 with {iter=t2}
    
    equation x1' = 1
    equation x2' = 2
    equation sum = x1+x2

    output y[t1] = (x1, x2, sum)
end
