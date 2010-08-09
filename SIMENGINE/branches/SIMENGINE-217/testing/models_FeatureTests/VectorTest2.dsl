model VectorTest2
    
    iterator i = 1..10

    state y[i] = 0
    equation y'[i] = y[i]

    state x[i] = 0
    equation x[i]' = x[i]

    state yn[i] = 0 with {iter = n}
    equation yn[n+1, i] = yn[n, i]

    state xn[i] = 0 with {iter = n}
    equation xn[n+1, i] = xn[n, i]

    solver=forwardeuler{dt=1}
end
