model VectorTest4 (x[i])
    
    state y[i] = 0
    equation y'[i] = x[i]

    solver=forwardeuler{dt=1}
end
