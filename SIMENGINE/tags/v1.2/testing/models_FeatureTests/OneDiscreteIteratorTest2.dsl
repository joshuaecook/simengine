model (x)=OneDiscreteIteratorTest2

    iterator int with {discrete}
    state x = 0 with {iter=int}
    
    equation x[int+1] = x[int] + 1
    
end
