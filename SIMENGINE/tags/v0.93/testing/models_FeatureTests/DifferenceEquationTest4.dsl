model (y)=DifferenceEquationTest4
    
    n {discrete, sample_frequency=1}
    state x = 0 with {iter=n}
    
    equation x[n+1] = x[n] + 1

    output y = (x, x[n], x[n-1], x[n-2])

end
