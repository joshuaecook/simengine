model (y)=DifferenceEquationTest3
    
    n {discrete, sample_frequency=1}
    state x = 0 with {iter=n}
    state x_delay1 = 0 with {iter=n}
    state x_delay2 = 0 with {iter=n}
    state x_delay3 = 0 with {iter=n}
    
    equation x[n+1] = x[n] + 1
    equation x_delay1[n+1] = x[n]
    equation x_delay2[n+1] = x[n-1]
    equation x_delay3[n+1] = x[n-2]

    output y = (x, x_delay1, x_delay2, x_delay3)

end
