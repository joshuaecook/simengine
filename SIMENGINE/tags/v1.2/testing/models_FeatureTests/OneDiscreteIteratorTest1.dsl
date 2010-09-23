model (x)=OneDiscreteIteratorTest1

    iterator n1 with {discrete}
    state x = 0 with {iter=n1}
    
    equation x[n1+1] = x[n1] + 1
    
end
