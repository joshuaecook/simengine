model (y)=PostProcessDiscreteIteratorTest2

    iterator n1 with {discrete}
    state x = 0 with {iter=n1}
    
    equation x[n1+1] = x[n1] + 1

    // Post Process States
    state x_d1 = 0 with{iter=n1}
    state x_d2 = 0 with{iter=n1}
    equations
	x_d1 = x
	x_d2 = x_d1
    end

    output y[n1] = (x, x_d1, x_d2)
    
end

