model (y)=DifferenceEquationTest6
    
    n {discrete, sample_frequency=1}
    state x = 1 with {iter=n}
    
    equation x[n+1] = x[n] + 1

    output y = x[n-2]

end
