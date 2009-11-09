model (x)=DifferenceEquationTest2

    state x = 0 with{iter=n, output_frequency=2}
    
    equation x[n+1] = x[n] + 0.5

end
