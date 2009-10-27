model (x)=UpdateDiscreteIteratorTest1

    state x = 0
    
    equation x[n+1] = x[n] + 1
    equation x = 0 when n >= 4

end
