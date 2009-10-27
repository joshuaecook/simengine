model (x)=UpdateDiscreteIteratorTest2

    iterator n1 with {discrete}
    state x = 0 with {iter=n1}
    
    equation x[n1+1] = x[n1] + 1
    equation x = 0 when n1 >= 4

end
