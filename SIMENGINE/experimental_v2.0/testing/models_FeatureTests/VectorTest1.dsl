model (x,y,xn,yn) = VectorTest1
    
    iterator i = 1..10

    state y[i] = 0
    equation y'[i] = 1

    state x[i] = 0
    equation x[i]' = 2

    state yn[i] = 0 with {iter=n}
    equation yn[n+1, i] = 1

    state xn[i] = 0 with {iter=n}
    equation xn[n+1, i] = 2

    solver=forwardeuler{dt=1}
end
