model (x)=UpdateContinuousIteratorTest3


    state x1 = 0
    state x2 = 0
    
    equations
	x1' = 1
	x2' = 1
    
	cond = x1 >= 4
	x1 = 0 when cond
	x2 = 0 when cond
    end

    output x = (x1, x2)
    solver=forwardeuler{dt=1}
end
	
