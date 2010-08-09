model (iter)=DifferenceEquationTest7

    state x = 0 with{iter=n}
    
    equation x[n+1] = x[n] + 1

    output iter = (n, n[-1])
end
