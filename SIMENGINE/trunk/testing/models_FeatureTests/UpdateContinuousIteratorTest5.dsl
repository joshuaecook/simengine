//settings.ir.aggregate.setValue(true)
model (x)=UpdateContinuousIteratorTest5

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=forwardeuler{dt=1}}

    state x1 = 0 with {iter=t1}
    state x2 = 0 with {iter=t1}
    state x3 = 0 with {iter=t2}
    
    equations
	x1' = 1
	x2' = 1
	x3' = 1
    
	cond = x1 >= 4
	x1 = 0 when cond
	x2 = 0 when cond
	x3 = 0 when cond
    end

    output x[t1] = (x1, x2, x3)
    solver=forwardeuler{dt=1}
end
	
