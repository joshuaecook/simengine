model (x)=DifferenceEquationTest2
    
    n {discrete, sample_frequency=2}
    state x = 0 with {iter=n}
    
    equation x[n+1] = x[n] + 0.5
end
