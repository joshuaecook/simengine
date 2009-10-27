model (y)=PostProcessDiscreteIteratorTest1

    state x = 0
    
    equation x[n+1] = x[n] + 1

    // Post Process States
    state x_d1 = 0
    state x_d2 = 0
    equations
	x_d1 = x
	x_d2 = x_d1
    end

    output y = (x, x_d1, x_d2)
    
end

